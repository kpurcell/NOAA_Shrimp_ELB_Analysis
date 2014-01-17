#  Powershell script to colate the manuscript.

# Render the bibliography in markdown
pandoc --biblio SpatialEffortStudy.bib --csl plos.csl SpatialEffortStudy_PLoS.md -o SpatialEffortStudy_PLoS.docx

# merge the FigsTables.md file with the manuscript+references
cat markdown.md, figtables.md > manuscript.md

# convert manuscript.md to .docx
pandoc SpatialEffortStudy_PLoS.md -o SpatialEffortStudy_PLoS.docx

