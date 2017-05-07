#include <cmath>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <limits>
#include <vector>

#include <ffi.h>
#include <dlfcn.h>

struct Value {
    ffi_type *type;
    union {
        uint8_t u8;
        uint16_t u16;
        uint32_t u32;
        uint64_t u64;
        int8_t s8;
        int16_t s16;
        int32_t s32;
        int64_t s64;
        float f;
        double d;
        void *ptr;
    } value;
};

std::ostream &operator<<(std::ostream &out, const Value &v) {
    if (v.type == &ffi_type_uint8) {
        out << v.value.u8;
    } else if (v.type == &ffi_type_uint16) {
        out << v.value.u16;
    } else if (v.type == &ffi_type_uint32) {
        out << v.value.u32;
    } else if (v.type == &ffi_type_uint64) {
        out << v.value.u64;
    } else if (v.type == &ffi_type_sint8) {
        out << v.value.s8;
    } else if (v.type == &ffi_type_sint16) {
        out << v.value.s16;
    } else if (v.type == &ffi_type_sint32) {
        out << v.value.s32;
    } else if (v.type == &ffi_type_sint64) {
        out << v.value.s64;
    } else if (v.type == &ffi_type_float) {
        float value = v.value.f;
        if(std::isnan(value)) {
            out << "0.0e+NaN";
        } else if(std::isinf(value)) {
            if(value < 0) {
                out << "-1.0e+INF";
            } else {
                out << "1.0e+INF";
            }
        } else {
            out << value;
        }
    } else if (v.type == &ffi_type_double) {
        double value = v.value.d;
        if(std::isnan(value)) {
            out << "0.0e+NaN";
        } else if(std::isinf(value)) {
            if(value < 0) {
                out << "-1.0e+INF";
            } else {
                out << "1.0e+INF";
            }
        } else {
            out << value;
        }
    } else if (v.type == &ffi_type_pointer) {
        out << "\\" << v.value.ptr;
    } else if (v.type == &ffi_type_void) {
        out << ":void";
    }
    out << "\n$";
    return out;
}

class FFIStack {
public:
    void push(uint8_t v) {
        stack_.push_back(Value{&ffi_type_uint8, {.u8 = v}});
    }
    void push(uint16_t v) {
        stack_.push_back(Value{&ffi_type_uint16, {.u16 = v}});
    }
    void push(uint32_t v) {
        stack_.push_back(Value{&ffi_type_uint32, {.u32 = v}});
    }
    void push(uint64_t v) {
        stack_.push_back(Value{&ffi_type_uint64, {.u64 = v}});
    }
    void push(int8_t v) {
        stack_.push_back(Value{&ffi_type_sint8, {.s8 = v}});
    }
    void push(int16_t v) {
        stack_.push_back(Value{&ffi_type_sint16, {.s16 = v}});
    }
    void push(int32_t v) {
        stack_.push_back(Value{&ffi_type_sint32, {.s32 = v}});
    }
    void push(int64_t v) {
        stack_.push_back(Value{&ffi_type_sint64, {.s64 = v}});
    }
    void push(double v) {
        stack_.push_back(Value{&ffi_type_double, {.d = v}});
    }
    void push(float v) {
        stack_.push_back(Value{&ffi_type_float, {.f = v}});
    }
    void push(void *v) {
        stack_.push_back(Value{&ffi_type_pointer, {.ptr = v}});
    }
    void push(Value &v) { stack_.push_back(v); }
    void push() {
        stack_.push_back(Value{&ffi_type_void, {0}});
    }

    Value pop() {
        Value v = stack_.back();
        stack_.pop_back();
        return v;
    }

    Value peek() { return stack_.back(); }

    Value &operator[](int i) { return stack_[stack_.size() - i - 1]; }

    void dup() {
        stack_.push_back(stack_.back());
    }

    size_t size() {
        return stack_.size();
    }

private:
    std::vector<Value> stack_;
};

class Machine {
public:
    Machine(std::ostream &out) : out_(out) {
        out.precision(std::numeric_limits<double>::digits10);
    };
    ~Machine() {
        for (auto &i : cifs_) {
            delete i->arg_types;
            delete i;
        }
        for (auto &i : libs_) {
            if (i) dlclose(i);
        }
    };

    FFIStack stack;

    void pop() { out_ << stack.pop(); }

    void peek() { out_ << stack.peek(); }

    void dlopen() {
        const char *name = static_cast<const char *>(stack.pop().value.ptr);
        void *handle = ::dlopen(name, RTLD_LAZY);
        stack.push(handle);
        libs_.push_back(handle);
    }

    void dlsym() {
        const char *name = static_cast<const char *>(stack.pop().value.ptr);
        void *handle = stack.pop().value.ptr;
        void *ptr = ::dlsym(handle, name);
        stack.push(ptr);
    }

    void cif() {
        ffi_cif *cif = new ffi_cif;
        uint32_t nargs = stack.pop().value.u32;
        ffi_type *rtype = stack.pop().type;
        ffi_type **types = new ffi_type*[nargs];
        for (uint32_t i = 0; i < nargs; i++) {
            types[i] = stack.pop().type;
        }
        ffi_prep_cif(cif, FFI_DEFAULT_ABI, nargs, rtype, types);
        stack.push(cif);
        cifs_.push_back(cif);
    }

    void call(bool get_errno) {
        void (*function)() = reinterpret_cast<void (*)()>(stack.pop().value.ptr);
        ffi_cif *cif = static_cast<ffi_cif *>(stack.pop().value.ptr);
        int nargs = cif->nargs;
        void *args[nargs];
        Value result {cif->rtype, {0}};
        for (int i = 0; i < nargs; i++) {
            args[i] = &stack[i].value;
        }
        if(get_errno)
            errno = 0;
        ffi_call(cif, function, &result.value, args);
        for (int i = 0; i < nargs; i++) {
            stack.pop();
        }
        if(get_errno)
            stack.push(errno);
        stack.push(result);
    }

    void strlen() {
        char *str = static_cast<char *>(stack.peek().value.ptr);
        uint32_t size = std::strlen(str);
        stack.push(size);
    }

    void dump() {
        uint32_t size = stack.pop().value.u32;
        char *data = static_cast<char *>(stack.pop().value.ptr);
        out_.write(data, size);
    }

    void read_mem() {
        int type = std::cin.get();
        Value out;
        uint8_t* ptr_bytes = (uint8_t*) stack.pop().value.ptr;
        size_t offset = stack.pop().value.u64;
        ptr_bytes -= offset;
        void* ptr = ptr_bytes;
        switch(type) {
        case 'u':
            out.type = &ffi_type_uint8;
            out.value.u8 = *static_cast<uint8_t*>(ptr);
            break;
        case 'v':
            out.type = &ffi_type_uint16;
            out.value.u16 = *static_cast<uint16_t*>(ptr);
            break;
        case 'w':
            out.type = &ffi_type_uint32;
            out.value.u32 = *static_cast<uint32_t*>(ptr);
            break;
        case 'x':
            out.type = &ffi_type_uint64;
            out.value.u64 = *static_cast<uint64_t*>(ptr);
            break;
        case 'i':
            out.type = &ffi_type_sint8;
            out.value.s8 = *static_cast<int8_t*>(ptr);
            break;
        case 'j':
            out.type = &ffi_type_sint16;
            out.value.s16 = *static_cast<int16_t*>(ptr);
            break;
        case 'k':
            out.type = &ffi_type_sint32;
            out.value.s32 = *static_cast<int32_t*>(ptr);
            break;
        case 'l':
            out.type = &ffi_type_sint64;
            out.value.s64 = *static_cast<int64_t*>(ptr);
            break;
        case 'f':
            out.type = &ffi_type_float;
            out.value.f = *static_cast<float*>(ptr);
            break;
        case 'd':
            out.type = &ffi_type_double;
            out.value.d = *static_cast<double*>(ptr);
            break;
        case 'p':
            out.type = &ffi_type_pointer;
            out.value.ptr = *static_cast<void**>(ptr);
            break;
        default:
            std::cerr << "invalid type in read_mem: " << static_cast<char>(type) << std::endl;
            return;
        }
        stack.push(out);
    }

    void free() { delete static_cast<char *>(stack.pop().value.ptr); }

    void size() {
        uint32_t size = stack.size();
        stack.push(size);
    }

private:
    std::ostream &out_;
    std::vector<ffi_cif *> cifs_;
    std::vector<void *> libs_;
};

class Reader {
public:
    Reader(Machine &vm, std::istream &in) : vm_(vm), in_(in) {};

    void read_uint8() {
        uint8_t i;
        in_ >> i;
        vm_.stack.push(i);
    }
    void read_uint16() {
        uint16_t i;
        in_ >> i;
        vm_.stack.push(i);
    }
    void read_uint32() {
        uint32_t i;
        in_ >> i;
        vm_.stack.push(i);
    }
    void read_uint64() {
        uint64_t i;
        in_ >> i;
        vm_.stack.push(i);
    }

    void read_sint8() {
        int8_t i;
        in_ >> i;
        vm_.stack.push(i);
    }
    void read_sint16() {
        int16_t i;
        in_ >> i;
        vm_.stack.push(i);
    }
    void read_sint32() {
        int32_t i;
        in_ >> i;
        vm_.stack.push(i);
    }
    void read_sint64() {
        int64_t i;
        in_ >> i;
        vm_.stack.push(i);
    }

    void read_float() {
        float f;
        in_ >> f;
        vm_.stack.push(f);
    }
    void read_double() {
        double d;
        in_ >> d;
        vm_.stack.push(d);
    }

    void read_pointer() {
        void *ptr = nullptr;
        in_ >> ptr;
        vm_.stack.push(ptr);
    }

    void read(uint32_t size) {
        char *buffer = new char[size + 1];
        buffer[size] = '\0';
        in_.read(buffer, size);
        vm_.stack.push(buffer);
    }

private:
    Machine &vm_;
    std::istream &in_;
};

int main() {
    Machine vm{std::cout};
    Reader reader{vm, std::cin};

    while (!std::cin.eof()) {
        int command = std::cin.get();
        switch (command) {
        /* push signed integer */
        case 'i' + 0: /* i */
            reader.read_sint8();
            break;
        case 'i' + 1: /* j */
            reader.read_sint16();
            break;
        case 'i' + 2: /* k */
            reader.read_sint32();
            break;
        case 'i' + 3: /* l */
            reader.read_sint64();
            break;

        /* push unsigned integer */
        case 'u' + 0: /* u */
            reader.read_uint8();
            break;
        case 'u' + 1: /* v */
            reader.read_uint16();
            break;
        case 'u' + 2: /* w */
            reader.read_uint32();
            break;
        case 'u' + 3: /* x */
            reader.read_uint64();
            break;

        /* push float */
        case 'f':
            reader.read_float();
            break;
        case 'd':
            reader.read_double();
            break;

        /* push pointer */
        case 'p':
            reader.read_pointer();
            break;

        /* push void */
        case 'V':
            vm.stack.push();
            break;

        /* peek */
        case 'e':
            vm.peek();
            break;

        /* pop */
        case 'o':
            vm.pop();
            break;

        /* call */
        case 'c':
            vm.call(false);
            break;

        /* call with errno */
        case 'E':
            vm.call(true);
            break;

        /* call */
        case 'C':
            vm.cif();
            break;

        /* "malloc" */
        case 'M':
            reader.read(vm.stack.pop().value.u32);
            break;

        /* free */
        case 'F':
            vm.free();
            break;

        /* dl calls */
        case 'O':
            vm.dlopen();
            break;
        case 'S':
            vm.dlsym();
            break;

        /* strlen */
        case 'L':
            vm.strlen();
            break;

        /* dump */
        case 'D':
            vm.dump();
            break;

        /* read from memory */
        case 'r':
            vm.read_mem();
            break;

        /* duplicate top value */
        case '!':
            vm.stack.dup();
            break;

        /* get stack size */
        case '?':
            vm.size();
            break;

        /* no-op */
        case ' ':
        case '\n':
        case '\r':
        case -1:
            break;

        default:
            std::cerr << "invalid op: " << static_cast<char>(command) << std::endl;
            break;
        }
        std::cout.flush();
    }

    std::cout << std::endl;
    return 0;
}
