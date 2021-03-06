# prubasic

A Clojure library designed compile a BASIC like language to the
instruction set of TI's PRUs (specifically the ones that are built
into the beaglebone)

## Usage

the function `prubasic.core/compile-basic` can be bassed a string of
BASIC code and it will return a byte array containing the compiled code

```clojure
prubasic.core>   (compile-basic
   "
LET temp = 0x0
LET fib = 0x1
FOR number = 0x1 TO 0x10
  LET pair = temp + fib
  LET temp = fib
  LET fib = pair
NEXT number
WRITE fib 0x0
END
")
#<byte[] [B@572c0127>
prubasic.core> 
```

This example program calculates fib(16) and writes the result as a
32bit integer to the base address of the PRU's data ram, where you can
read it out afterwards. 

the BASIC dialect has 8 commands:
  - LET
  - FOR
  - NEXT
  - IF
  - GOTO (targets a label)
  - END
  - READ
  - WRITE
  - labels of the form FOO:

READ/WRITE are for writing to the PRU's data ram, which can be used to
communicate results or pass arguments. They take a variable to be
written to and an offset to read/write from, offsets are in bytes, so
for 32bit ints use units of 4.

Speaking of variables, the compiler just maps names to registers, so
don't use more than 28 variables (the compiler uses a few of the 32
general purpose registers for bookkeeping). The compiler may or may
not try to be smart about reusing a register when the variable that
was assigned to it is no longer used.

The only value types currently supported are 32bit integers which must
be written in hex.

The only operator on values is currently +.

Really there is just enough functionality here to make fib work.

Loading the generated code in to the PRU is still kind of a pain,
hopefully that will get better soon. See https://github.com/hiredman/beaglebone-pru-swig

## Reading

  - https://github.com/beagleboard/am335x_pru_package/blob/master/am335xPruReferenceGuide.pdf
  - http://processors.wiki.ti.com/index.php/PRU_Assembly_Instructions#Register_Load_and_Store

## License

Copyright © 2014 Kevin Downey

Distributed under the Eclipse Public License, the same as Clojure.
