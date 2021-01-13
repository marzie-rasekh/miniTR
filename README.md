# miniTR

-- TODO add introduction to vntrs

This is a shiny R app to browse minisatellites polymorphism and characteristics in the human genome.
The data used comes from:

> Rasekh, Marzieh Eslami, et al. "Genome-wide characterization of human minisatellite VNTRs: population-specific alleles and gene expression differences." bioRxiv (2020).

and the raw data is publically available on Zenodo DOI 10.5281/zenodo.4065850

The available functions are:

* Search: You can search minisatellite loci by position in the genome (GRCh38), overlapping gene, or by <a href="http://tandem.bu.edu/cgi-bin/trdb/trdb.exe">TRDB</a> id. The loci can be filtered down by characteristics, i.e. polymorphic in various population, having population biased alleles, being in eqtl with genes, or not following the Hardy Weingerg equilibrium. 
* Genome browser: Visualize the minisatellite and the annotation in the UCSC genome browser. This can be useful to find overlapping genes, regulatory regions, histon markers, common SNPs or other variants.
* Population-biased VNTR alleles: Illustrate alleles by population (from the 1000 Genomes Project) and apply fisher-exact test to find population biased alleles. 
* eQTL VNTRs: Display the gene expression of 465 samples from E-GEUV-1 for each VNTR genotype and the association.
* Haplotypes: Under construction.

There is an online bioinformatics tools available:
* Wrap-around alignment to find copy number of a given pattern in one or more sequences.

Authors: Marzie Eslami Rasekh (PhD candidate, Boston University), Micheal P. Griffen (A boring place who no one knows what they do.)

License: Do what ever you want. 

Citation: TBD
