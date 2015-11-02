Wosa
====

This is Wosa. Wosa stands for "WordSet Assembler" and has been written as an
assistance tool for users in putting together wordsets respecting specific
constraints. Wosa implements basic program logic while the real work, including
a user interface, is to be implemented by a backend module.

Backend: Nebelfiller
====================

At the moment there exists only one backend module, which is called
"Nebelfiller". It was successfully used in the the context of a
psycholinguistics research project, where a list of many wordset, quadruples to
be precise, had to be assembled.

Status of the Code
==================

Wosa/Nebelfiller was written for a very specific purpose. I have tried to
organize the code in a way which, in principle, allows for the adoption of the
codebase by plugging in a different backend module (the included Nebelfiller
backend might serve as a template).

Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>
