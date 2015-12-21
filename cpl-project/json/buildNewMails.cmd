@echo off
ECHO nothing > newmails.json
ping -n 11 127.0.0.1 > nul
CAT newmails1.json > newmails.json
ping -n 6 127.0.0.1 > nul
CAT newmails2only.json > newmails.json