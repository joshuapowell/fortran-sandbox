<a href="https://travis-ci.org/joshuapowell/fortran-sandbox" target="_blank">
<img src="https://travis-ci.org/joshuapowell/fortran-sandbox.svg?branch=master" />
</a>

# Fortran Sandbox

## Compile
To compile the `hello.f90` example program you will need to have the
`gfortran`.

### Install on macOS
The `gcc` Homebrew package comes with the `gfortan` program.

To install `gcc` with `gfortran`

```
brew install gcc
```

To verify that `gfortran` has been installed run `gfortran --version` and
press enter. You should receive a message similar to the following.

```
GNU Fortran (Homebrew GCC 8.3.0) 8.3.0
```

## Todo

- [ ] Add installation instructions for Linux
- [ ] Add installation instructions for Windows
- [ ] Add alternative `gfortran` compile instructions.

## Testing with fUnit
Example unit test runner
```
funit circle_class
```

### Installing Unit Testing Software
`fUnit` is a Fortran unit testing framework developed by engineers at NASA. It 
requires a Fortran 95 compiler and it is designed for testing routines 
contained in modules. fUnit is written in Ruby and is distributed as a Ruby 
Gem at RubyForge.

`fUnit` can be installed via a Ruby gem
```
gem install funit
```

After installation ensure you've set the FC environment variable to your
fortran compiler. In addition, you will need to make sure other flags are
properly set as well (I can't explain this part yet.).
```
export FC="gfortran"
export FSFLAG=-I
export CXX="g++"
export CC="gcc"
```

https://jblevins.org/log/funit
