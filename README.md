# phagepop

On Linux, I needed to add the following to my ~/.octaverc file to load image and make plot lines thick enough to see.

------------begin ~/.octaverc

pkg load image;

set(0, "defaultaxeslinewidth", 1.0);

set(0, "defaultlinelinewidth", 1.0);

------------end ~/.octaverc



Phagestat population dynamic model for EvoStat:
a realization of Husimi's CellStat (PhageStat).

modrun.m     - solves the differential equations (in f.m) and plots them.
jpfill.m     - colors between two curves to showing bounds of activity/population levels in our model.
colortable.m - a utility to show the default colors for plot lines.

f.m          -  models the system of five differential equations from:

Husimi:
  "Cellstat—A continuous culture system of a bacteriophage for the study of
   the mutationnrate and the selection process at the DNA level"

In Review of Scientific Instruments 53, 517 (1982);

Yuzuru Husimi, Koichi Nishigaki, Yasunori Kinoshita, and Toyosuke Tanaka
doi: 10.1063/1.1137002
View online: http://dx.doi.org/10.1063/1.1137002

An updated (1989) version is: "Selection and evolution of bacteriophages in cellstat"
Adv Biophys. 1989;25:1-43.

Abstract:  Objectives of this work were as follows: 1. to establish a laboratory experimental system utilizable in a biophysical approach to molecular evolution; and 2. to provide real world parameters to theories of molecular evolution, especially to Eigen's theory of quasi-species. Secretion type bacteriophage fd of E. coli, closely related phages and artificial chimera phages of fd, and a virulent phage Q beta of E. coli were cultured continuously in a specially designed fermenter called a "cellstat". A phage is cultured in a flow of host bacterial cells. Due to its high dilution rate, the mutant cell could not be selected in the cellstat. It was therefore recognized that the cellstat is suitable for study of the selection and evolution process of a bacteriophage under well-defined environmental conditions without interference from host cell mutations. Population dynamics of bacteriophages of various types in the cellstat were studied theoretically by computer simulation and experimentally. A genetically invariable pure population of phage behaves like an open non-linear chemical reaction system. An invariable mixed population shows a selection process, while a variable population generates an evolution process. Kinetic constants describing the dynamics were determined by curve fitting between the theoretical and the experimental curve obtained from competition experiments and from biological relaxation experiments. One of the most important kinetic parameters thus obtained was the selection coefficient, and its dependence on the base sequence of phage DNA. We drew a local landscape of the selection coefficient near the fd sequence on the base sequence space. From this landscape we were able to confirm the importance of slightly deleterious mutants in molecular evolution. We also confirmed the possibility of developing an evolutionary molecular engineering using a cellstat as an evolution reactor and fd phage as a working replicon. Novelties of this work were as follows: 1. the first stable continuous culture of a bacteriophage was achieved with a cellstat; 2. a local landscape of selection coefficient near the fd sequence on the sequence space was the first experimental drawing of such a map; 3. a biological relaxation method was realized to measure kinetic constants of a biological kinetic process, or molecular evolution; and 4. a practical engineering process of evolutionary molecular engineering was proposed.
