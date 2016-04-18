# gsxsteg

A small commandline program which provides steganography.

## License

GPL

## Usage

`gsxsteg [-batch] -<action> [-<argument> <value>]`
 
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
