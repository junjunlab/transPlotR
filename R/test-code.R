library(transPlotR)
data(gtf)

# non-coding gene
trancriptVis(gtfFile = gtf,
             gene = 'Xist')

# coding gene
trancriptVis(gtfFile = gtf,
             gene = 'Nanog')

# change fill color
trancriptVis(gtfFile = gtf,
             gene = 'Nanog',
             exonFill = '#CCFF00')

# change inrton line size
trancriptVis(gtfFile = gtf,
             gene = 'Nanog',
             intronSize = 1)

# change label size,color and position
trancriptVis(gtfFile = gtf,
             gene = 'Nanog',
             textLabelSize = 4,
             textLabelColor = 'red',
             relTextDist = 0)

# aes by gene name
trancriptVis(gtfFile = gtf,
             gene = 'Nanog',
             textLabel = 'gene_name')

# color aes by transcript
trancriptVis(gtfFile = gtf,
             gene = 'Tpx2',
             exonColorBy = 'transcript_id')

# change arrow color and type
trancriptVis(gtfFile = gtf,
             gene = 'Nanog',
             arrowCol = 'orange',
             arrowType = 'closed')

# no intron gene and add arrow color
# change arrow color and type
trancriptVis(gtfFile = gtf,
             gene = 'Jun',
             textLabel = 'gene_name',
             arrowCol = 'white',
             arrowType = 'closed') +
  theme_void()

# add arrow breaks
trancriptVis(gtfFile = gtf,
             gene = 'Nanog',
             arrowCol = 'orange',
             arrowType = 'closed',
             arrowBreak = 0.1)

# draw specific transcript
p1 <- trancriptVis(gtfFile = gtf,
                   gene = 'Commd7')

p2 <- trancriptVis(gtfFile = gtf,
                   gene = 'Commd7',
                   myTranscript = c('ENSMUST00000071852','ENSMUST00000109782'))

# combine
cowplot::plot_grid(p1,p2,ncol = 2,align = 'hv')

########################################################
# add specific arrow
pneg <- trancriptVis(gtfFile = gtf,
                     gene = 'Gucy2e',
                     newStyleArrow = T)

ppos <- trancriptVis(gtfFile = gtf,
                     gene = 'Tex15',
                     newStyleArrow = T)

# combine
cowplot::plot_grid(pneg,ppos,ncol = 2,align = 'hv')

# remove normal arrow specific arrow
trancriptVis(gtfFile = gtf,
             gene = 'Fat1',
             newStyleArrow = T,
             addNormalArrow = F)

# draw absolute
trancriptVis(gtfFile = gtf,
             gene = 'Fat1',
             newStyleArrow = T,
             addNormalArrow = F,
             absSpecArrowLen = T)

# change position size color and height
trancriptVis(gtfFile = gtf,
             gene = 'Fat1',
             newStyleArrow = T,
             addNormalArrow = F,
             speArrowRelPos = 0.5,
             speArrowLineSize = 1,
             speArrowCol = 'red',
             speArrowRelHigh = 3)

# circle plot with specific arrow
trancriptVis(gtfFile = gtf,
             gene = 'F11',
             newStyleArrow = T,
             addNormalArrow = F,
             circle = T,
             ylimLow = -2)

########################################################
# support multiple gene
# should on same chromosome and close to each other
trancriptVis(gtfFile = gtf,
             gene = c('Trmt6','Mcm8','Crls1','Lrrn4','Fermt1'),
             textLabel = 'gene_name')

# color by gene and change arrow length
trancriptVis(gtfFile = gtf,
             gene = c('Crls1','Fermt1'),
             textLabel = 'gene_name',
             exonColorBy = 'gene_name',
             newStyleArrow = T,
             speArrowRelLen = 1)

# collapse gene
trancriptVis(gtfFile = gtf,
             gene = c('Trmt6','Mcm8','Crls1','Lrrn4','Fermt1'),
             textLabel = 'gene_name',
             collapse = T,
             relTextDist = 0.2)

########################################################
# support plot at a given region
trancriptVis(gtfFile = gtf,
             Chr = 11,
             posStart = 69609973,
             posEnd = 69624790)

########################################################
# draw circle structure
trancriptVis(gtfFile = gtf,
             gene = 'Gucy2e',
             textLabelSize = 4,
             circle = T)

# change circle small
trancriptVis(gtfFile = gtf,
             gene = 'Gucy2e',
             textLabelSize = 4,
             circle = T,
             ylimLow = 0)

# change circle angle
c1 <- trancriptVis(gtfFile = gtf,
                   gene = 'F11',
                   textLabelSize = 4,
                   circle = T,
                   ylimLow = 0,
                   openAngle = 0)

c2 <- trancriptVis(gtfFile = gtf,
                   gene = 'F11',
                   textLabelSize = 4,
                   circle = T,
                   ylimLow = 0,
                   openAngle = 0.2)

# combine
cowplot::plot_grid(c1,c2,ncol = 2,align = 'hv')

# chenge aes fill
trancriptVis(gtfFile = gtf,
             gene = 'Gucy2e',
             textLabelSize = 4,
             circle = T,
             ylimLow = 0,
             exonColorByTrans = T)

# change segment color
trancriptVis(gtfFile = gtf,
             gene = 'Gucy2e',
             textLabelSize = 4,
             circle = T,
             ylimLow = 0,
             exonColorByTrans = T,
             circSegCol = 'black')

# add gene name
trancriptVis(gtfFile = gtf,
             gene = 'Gucy2e',
             textLabel = 'gene_name',
             textLabelSize = 5,
             circle = T,
             ylimLow = 0,
             exonColorByTrans = T)

# remove line
trancriptVis(gtfFile = gtf,
             gene = 'Gucy2e',
             textLabel = 'gene_name',
             textLabelSize = 5,
             circle = T,
             ylimLow = 0,
             exonColorByTrans = T,
             text_only = T)

# multiple gene
trancriptVis(gtfFile = gtf,
             gene = c('Pfn1','Eno3','Spag7'),
             textLabel = 'gene_name',
             textLabelSize = 2,
             circle = T,
             ylimLow = -5,
             text_only = T,
             circSegCol = 'grey80',
             exonColorByTrans = T)
