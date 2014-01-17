#  Powershell script to colate the manuscript.

# Render the bibliography in markdown
pandoc --biblio SpatialEffortStudy.bib --csl plos.csl SpatialEffortStudy_PLoS.md -o SpatialEffortStudy_PLoS_bib.md

# merge the FigsTables.md file with the manuscript+references
cat SpatialEffortStudy_PLoS_bib.md, SpatialEffortStudy_FigTables.md > manuscript.md

# convert manuscript.md to .docx
pandoc manuscript.md -o Purcell_etal_PLoS.docx

