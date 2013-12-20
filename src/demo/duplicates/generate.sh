#!/bin/bash


mkdir fdl_copy
mkdir fdl

for i in {1..5}
do
   type NUL > 'documentation-plt'.pdf
   mv documentation-plt.pdf fdl_copy

   type NUL > 'documentation-plt'.tex
   mv documentation-plt.tex fdl_copy

   type NUL > 'presentation-plt'.ppt
   mv presentation-plt.ppt fdl_copy

   type NUL > 'presentation-plt'.doc
   mv presentation-plt.doc fdl_copy

   type NUL > 'presentation-plt'.docx
   mv presentation-plt.docx fdl_copy
done

for i in {1..5}
do
   type NUL > 'documentation'.pdf
   mv documentation.pdf fdl

   type NUL > 'documentation'.tex
   mv documentation.tex fdl

   type NUL > 'presentation-plt'.ppt
   mv presentation-plt.ppt fdl

   type NUL > 'presentation-plt'.doc
   mv presentation-plt.doc fdl

   type NUL > 'presentation-plt'.docx
   mv presentation-plt.docx fdl
done