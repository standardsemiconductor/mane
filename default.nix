{ mkDerivation, base, bytestring, ftdi, optparse-applicative
, stdenv, usb, vector
}:
mkDerivation {
  pname = "mane";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring ftdi optparse-applicative usb vector
  ];
  description = "VELDT Programmer";
  license = stdenv.lib.licenses.mit;
}
