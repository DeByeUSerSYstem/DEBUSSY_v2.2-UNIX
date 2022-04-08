#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Copyright (c) 2015 Antonio Cervellino, Ruggero Frison, Federica Bertolotti, Antonietta Guagliardi
#
#     This file is part of the Claude / Debussy program suite
#     
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 3 of the License (29 June 2007), 
#     or (at your option) any later version.
#     
#     The Claude / Debussy program suite is distributed in the hope that it will be 
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License, version 3, 29 June 2007 for more details.
#     
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA or
#     see <http://www.gnu.org/licenses/>.
#
#===================================================================================================
welcomeText = '         ***** WELCOME to DebUsSy ! ***** \n  a Debye User System for nanocrystalline and non ordered materials \n \n \
        ________INSTRUCTIONS_________  \n \
(for details please have a look at Claude and Debussy manuals)  \n \n \
use "Set Job" to set your working folder where .. \n \
1 . .. build a database of atomic clusters with CLAUDE suite of programs \n \
  a. make the Unit "CELL" (*pha, *gen files required) \n \
  b. make the sphere/rod shaped "XYZ" clusters (DB_CLU_Info.inp and DB_Phase_Info.inp files required) \n \
  c. "Build DB" (database) of sampled interatomic distances for a crystalline phase (*pha, *gen, DB_CLU_Info.inp and DB_Phase_Info.inp files required) \n \
  d. build a database of sampled interatomic distances for a non-crystalline phase - "MOLEC" - (xyz files required) \n \
  e. calculate the diffraction "PATTERN" of a single cluster (diffractor.inp and *smp files required) \n \
2. .. calculate the Debye Scattering Equation from a database with DEBUSSY \n \
  a. perform a "Simulation" (*smp, *dwa, *pha and experimenta data required) \n \
  b. perform a "Refinement" (*smp, *dwa, *pha and experimenta data required) \n \
  c. perform the calculation of the "STD" (standard deviations) of the refined parameters (*smp, *dwa, *pha and experimental data required) \n \
3. .. or find the files to PLOT \n \
  a. a single "XYZ" cluster with Jmol (*xyz_NEW file required)\n \
  b. the diffraction "Pattern" of a single cluster (*tqi file required) \n \
  c. the experimental "Data" \n \
  d. the "Simulation" diffraction patterns \n \
  e. the "Best fit" diffraction patterns \n \
  f. the number and mass size "Distributions" \n \
  g. the size dependent "S.O.F." (Site Occupation Factors) \n \
  h. the size dependent "D.W." (Debye-Waller factors) \n \
~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~\n'
