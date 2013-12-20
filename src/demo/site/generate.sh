#!/bin/bash

for i in {1..20}
do
   type NUL > 'index-'$i.html
   type NUL > 'script-'$i.js
   type NUL > 'img-'$i.jpeg
   type NUL > 'css-'$i.css
done