# DA🅱️: Dank Assembly 🅱️
All commands are capitalized
Comments are any text on a line beyond what the interpreter expects (it doesn't care)

##Commands:
* "Load" loads any value into a given register.
  Example:
  * Load 10 a --Puts the value of 10 into a
  * Load a b  --Puts the value of a into b
  * Load (30) a --Puts the value at memory location 30 into b
  * Load * a --Puts the value of memory X is pointing at into a
* "Write" writes a register value into memory.
  Example:
  * Write a 30 --Puts the value of a into memory location 30
* "Add" adds two values and stores them in a register
  Example:
  * Add 1 1 a --Computes 1+1 and stores in a
  * Add a 1 a --Computes a+1 and stores in a
  * Add (30) * x --Dereferences 30 and x, adds them, and stores in x
* "Sub" all other arithmetic operations are the same
* "Mul"
* "Div"
* "Mod"
* "TLT" Takes two values, and stores their comparison (less than) in t
  Example:
  * "TLT 10 9" loads 1 into t
  * "TLT 9 10" loads 0 into t
  * "TLT a b" Loads a < b into t
* "TEQ" Same as TLT but for equality
* "Scan" performs a hardware scan instruction using a and b as the angle
         arguments. It loads the number of friendly tanks found into a
          and the number of hostile tanks found into b.
* "Fire" If you need documentation for this, please seek help
* "Gyro" loads the current orientation of the tank into a
* "Move" advances the tank 10 pixels
* "Aim" sets the orientation of the tank to the value in a starting from 0
* "GPS" loads the x chunk of the tank into a and the y chunk of the tank into b
* "Jmp" takes a value and causes the execution to resume from that index in the program
  Example:
   * Jmp 5
   * Jmp a
   * Jmp (40)
   * Jmp *
* "JmpIf" acts as Jmp when t is 1, but does not do anything when t is 0
* "Nop" does nothing
##Values:
A value is either a
1. Register label (a,b,t,x), and acts as the value of that register
2. Memory location (_) where _ is a literal, and acts as the value in memory at that location
3. Pointer dereference, *, and acts as the value that the X register points to
4. A literal
