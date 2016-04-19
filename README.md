# gsxsteg

A small commandline program which provides steganography.

## License

GPL

## Compatibility

OS
: Windows, Linux

Compiler
: Free Pascal, Delphi

## Usage

~~~
gsxsteg [-batch] -<action> [-<argument> <value>]
 
  batch: Batch mode, everything must be given with commandline arguments
action can be:
  hide: Hide data inside a medium
  extract: Extract data from a medium
argument can be:
  medium: The medium file
  dest: The destination file
  msg: Filename with message to hide
  pass: Password
Example:
  gsxsteg -batch -extract -medium mymedium.png -dest thedata.txt -pass "my secret pass"
~~~

## Compiling

Delphi
: Open `gsxsteg.dproj` and click Compile

Lazarus
: Open `gsxsteg.lpi` and click Start -> Compile or type `lazbuild ./gsxsteg.lpi --build-all --build-mode=Release`

## Details

1. Make an array with possible coordinates.
2. Seed a pseudo random number generator (ISAAC) with the SHA-512 hash of the
  password.
3. Make a random key with this random numbers.
4. Seed a random generator with this random key.
5. Randomize the array with coordinates with the random number generator seeded
  with the key.
6. Use the coordinates in this array to store the data.
7. Store the data in the least significant bit of each color of the pixel.

So depending on the password you use you may get different data. If the wrong
password is used, you'll get most likely just random data. But it does not say
you there is no data. This is useful if the data is additionally encrypted.

Another advantage of this method is that it chooses the pixel to store the data
randomly, so the data is spread all over the image.
