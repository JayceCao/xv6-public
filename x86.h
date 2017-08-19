// Routines to let C code use special x86 instructions.

#ifndef _DEFS_H
#include "types.h"
#endif

/* Put value of the variable port into %edx,
 * read one byte from the port into %eax, then into variable data */
static inline uchar
inb(ushort port)
{
  uchar data;
  asm volatile("in %1,%0" : "=a" (data) : "d" (port));
  return data;
}

/* "cld", clear the direction flag(DF), "rep", repeat executing the following 
 * instruction until %ecx becomes zero */
static inline void
insl(int port, void *addr, int cnt)
{
  asm volatile("cld; rep insl" :
               "=D" (addr), "=c" (cnt) :
               "d" (port), "0" (addr), "1" (cnt) :
               "memory", "cc");
}

/* Put value of data into %eax, then put the port. The port number put into %edx.
 * the size of data is one byte*/
static inline void
outb(ushort port, uchar data)
{
  asm volatile("out %0,%1" : : "a" (data), "d" (port));
}

/* The same as above, while the size of data is one word */
static inline void
outw(ushort port, ushort data)
{
  asm volatile("out %0,%1" : : "a" (data), "d" (port));
}

static inline void
outsl(int port, const void *addr, int cnt)
{
  asm volatile("cld; rep outsl" :
               "=S" (addr), "=c" (cnt) :
               "d" (port), "0" (addr), "1" (cnt) :
               "cc");
}

/* Put one byte of the string in %eax into address addr */
static inline void
stosb(void *addr, int data, int cnt)
{
  asm volatile("cld; rep stosb" :
               "=D" (addr), "=c" (cnt) :
               "0" (addr), "1" (cnt), "a" (data) :
               "memory", "cc");
}

/* The same as above, while four bytes of %eax */
static inline void
stosl(void *addr, int data, int cnt)
{
  asm volatile("cld; rep stosl" :
               "=D" (addr), "=c" (cnt) :
               "0" (addr), "1" (cnt), "a" (data) :
               "memory", "cc");
}

struct segdesc;

/* Loads the values in the source operand into the global descriptor table register. */
static inline void
lgdt(struct segdesc *p, int size)
{
  volatile ushort pd[3];

  pd[0] = size-1;
  pd[1] = (uint)p;
  pd[2] = (uint)p >> 16;

  asm volatile("lgdt (%0)" : : "r" (pd));
}

struct gatedesc;

/* Loads the values in the source operand into the global interrupt descriptor register */
static inline void
lidt(struct gatedesc *p, int size)
{
  volatile ushort pd[3];

  pd[0] = size-1;
  pd[1] = (uint)p;
  pd[2] = (uint)p >> 16;

  asm volatile("lidt (%0)" : : "r" (pd));
}

/* Loads the source operand into the segment selector field of the task register */
static inline void
ltr(ushort sel)
{
  asm volatile("ltr %0" : : "r" (sel));
}

/* Save the status of eflags into variable eflags */
static inline uint
readeflags(void)
{
  uint eflags;
  asm volatile("pushfl; popl %0" : "=r" (eflags));
  return eflags;
}

/* Put the value of v into %gs */
static inline void
loadgs(ushort v)
{
  asm volatile("movw %0, %%gs" : : "r" (v));
}

/* If protected-mode virtual interrupts are not enabled,
 * CLI clears the IF flag in the EFLAGS register.
 * No other flags are affected.
 * Clearing the IF flag causes the processor to ignore maskable external interrupts.
 * The IF flag and the CLI and STI instruction have no affect on the generation
 * of exceptions and NMI interrupts */
static inline void
cli(void)
{
  asm volatile("cli");
}

/* Be opposite to "cli" */
static inline void
sti(void)
{
  asm volatile("sti");
}

/* "Lock", causes the processor’s LOCK# signal to be asserted
 * during execution of the accompanying instruction.
 * Put the value of newval into %eax, then exchange values of addr and %eax. */
static inline uint
xchg(volatile uint *addr, uint newval)
{
  uint result;

  // The + in "+m" denotes a read-modify-write operand.
  asm volatile("lock; xchgl %0, %1" :
               "+m" (*addr), "=a" (result) :
               "1" (newval) :
               "cc");
  return result;
}

/* Put the value of %cr2 into variable val.
 * %cr2 contains page fault information. */
static inline uint
rcr2(void)
{
  uint val;
  asm volatile("movl %%cr2,%0" : "=r" (val));
  return val;
}

/* %cr3 contains page directory information. */
static inline void
lcr3(uint val)
{
  asm volatile("movl %0,%%cr3" : : "r" (val));
}

//PAGEBREAK: 36
// Layout of the trap frame built on the stack by the
// hardware and by trapasm.S, and passed to trap().
struct trapframe {
  // registers as pushed by pusha
  uint edi;
  uint esi;
  uint ebp;
  uint oesp;      // useless & ignored
  uint ebx;
  uint edx;
  uint ecx;
  uint eax;

  // rest of trap frame
  ushort gs;
  ushort padding1;
  ushort fs;
  ushort padding2;
  ushort es;
  ushort padding3;
  ushort ds;
  ushort padding4;
  uint trapno;

  // below here defined by x86 hardware
  uint err;
  uint eip;
  ushort cs;
  ushort padding5;
  uint eflags;

  // below here only when crossing rings, such as from user to kernel
  uint esp;
  ushort ss;
  ushort padding6;
};
