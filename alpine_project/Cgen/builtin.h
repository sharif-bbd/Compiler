#ifndef BUILTIN_H
#define BUILTIN_H


#define PRINT(S) printf(_Generic((S),  \
                        double: "%f\n", \
                        float:  "%f\n", \
                        char *: "%s\n", \
                        int:    "%d\n"  \
                ), (S));

#define IADD(a, b) ((a) + (b))
#define ISUB(a, b) ((a) - (b))
#define IMUL(a, b) ((a) * (b))
#define IDIV(a, b) ((a) / (b))
#define IREM(a, b) ((a) % (b))
#define EXIT(a) exit(a)
#define LNOT(a) (!(a))
#define LAND(a, b) ((a) && (b))
#define LOR(a, b) ((a) || (b))

#define INEG(a) (-(a))
#define ISHL(a, b) ((a) << (b))
#define ISHR(a, b) ((a) >> (b))

#define ILT(a, b) ((a) < (b))
#define ILE(a, b) ((a) <= (b))
#define IGT(a, b) ((a) > (b))
#define IGE(a, b) ((a) >= (b))

#define IINV(a) (~(a))
#define IAND(a, b) ((a) & (b))
#define IOR(a, b) ((a) | (b))
#define IXOR(a, b) ((a) ^ (b))

#define FNEG(a) (-(a))
#define FADD(a, b) ((a) + (b))
#define FSUB(a, b) ((a) - (b))
#define FMUL(a, b) ((a) * (b))
#define FDIV(a, b) ((a) / (b))

#define EQUALITY(a,b) ((a) == (b))
#define FLT(a, b) ((a) < (b))
#define FLE(a, b) ((a) <= (b))
#define FGT(a, b) ((a) > (b))
#define FGE(a, b) ((a) >= (b))

#endif /* BUILTIN_H */