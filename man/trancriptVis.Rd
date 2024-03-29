% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trancriptVis.R
\name{trancriptVis}
\alias{trancriptVis}
\title{trancriptVis}
\arguments{
\item{gtfFile}{GTF file.}

\item{gene}{Target gene to plot.}

\item{myTranscript}{Specify which transcripts to plot use transcipt id.}

\item{Chr}{Chromosome number.}

\item{posStart}{Region start position on genome.}

\item{posEnd}{Region end position on genome.}

\item{collapse}{Whether to collapse multiple transcripts into one, default(FALSE).}

\item{exonWidth}{Exon width to plot, default(0.3).}

\item{relTextDist}{Transcripts name or gene name relative to exon, default(0.3).}

\item{intronSize}{Intron line size, default(0.5).}

\item{arrowBreak}{How many gap distance to draw arrows, the smaller the more arrows, default(0.15).}

\item{exonColorBy}{Whether color group by "transcript_id" or "gene_name", default(NULL).}

\item{exonFill}{Exon fill color, default('#333399').}

\item{circle}{Whether make plot into a circle plot, default(FALSE).}

\item{cicStart}{Circle plot start position, default(pi).}

\item{circSegCol}{Circle sgement color, default('#333399').}

\item{text_only}{When circle plot labeled by gene name, whether remove the line connected with gene name, default(FALSE).}

\item{ylimLow}{The Y axis lower limitation of Circle plot, default(-10).}

\item{openAngle}{The gap of the circle plot, default(0.5).}

\item{arrowCol}{Normal arrow color, default('#333399').}

\item{arrowAngle}{Normal arrow angle, default(30).}

\item{arrowLength}{Normal arrow length, default(0.1).}

\item{arrowType}{Normal arrow type, default('open').}

\item{addNormalArrow}{Whether add normal arrow on plot, default(TRUE).}

\item{newStyleArrow}{Whether add new style arrow on plot, default(FALSE).}

\item{absSpecArrowLen}{Whether make new style arrow length to be relative to each transcript length or absolute length to the longest transcript, default(FALSE).}

\item{speArrowRelPos}{The relative position to the transcript on horizontal direction of new style arrow, default(0).}

\item{speArrowRelLen}{The relative length to the transcript length of new style arrow, default(0.05).}

\item{speArrowStart}{The new style arrow start position on the vertical direction, default(-0.15).}

\item{speArrowRelHigh}{The relative height of new style arrow to the vertical length, default(2).}

\item{speArrowLineSize}{The new style arrow line size, default(0.5).}

\item{speArrowCol}{The new style arrow line color, default('black').}

\item{speArrowAngle}{The new style arrow angle, default(30).}

\item{speArrowLen}{The new style arrow length, default(0.1).}

\item{speArrowType}{The new style arrow type, default('closed').}

\item{textLabel}{The text label aesthetic mappings, default('transcript_id').}

\item{textLabelSize}{The text label size, default(5).}

\item{textLabelColor}{The text label color, default('black').}

\item{base_size}{Theme basesize, default(14).}

\item{marginX}{Plot left and right margins, default(0.2).}

\item{marginY}{Plot top and bottomn margins, default(0.2).}

\item{aspect.ratio}{Plot ratio, default(NULL).}

\item{facetByGene}{Whether facet by gene to plot, this useful for your genes which are far away from each other or not located on the same chromosome, default(FALSE).}

\item{ncolGene}{The column numbers to plot, default(NULL).}

\item{scales}{Facet plot scales, same as "facet_wrap" function, default('free').}

\item{strip.position}{Facet plot strip.position, same as "facet_wrap" function, default('top').}

\item{forcePosRel}{Whether force the genome coordinate to relative position to transcript start/end position, default('FALSE').}

\item{panel.spacing}{Facet plot panel space, default(0.3).}

\item{revNegStrand}{Whether reverse the negtive strand when set "forcePosRel=TRUE", default('FALSE').}

\item{xAxis.info}{Whether retain X axis ticks and text, default(TRUE).}

\item{reverse.y}{whether reverse the Y axis, default(FALSE).}

\item{text.pos}{the label position(left/right), default(middle).}

\item{selecType}{choose the representative transcript to show("lt(longest transcript)" or "lcds(longest CDS)"), default(NULL).}

\item{topN}{the top number representative transcript to be shown, default(1).}

\item{show.legend}{whether show color legend, default(FALSE).}
}
\value{
A ggplot object.
}
\description{
This package is to visualize gene diffrent isoforms.
}
\examples{
##############################################################
# test function

########################################################
# load data
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
}
\author{
JunZhang
}
