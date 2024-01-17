import ARCANE.tools as tools
import ARCANE.mechanisms as mechanisms
import ARCANE.custom.custom_kinetics as custom
import os


import cantera as ct

import scipy.optimize as opt
import numpy as np


# mech = mechanisms.Mechanism("C1skeletal.cti")
# mech = mechanisms.Mechanism("fake.cti")
# mech = mechanisms.Mechanism("YAO_reduced.cti")
# mech = mechanisms.Mechanism("YAO_reduced.cti")
mech = mechanisms.Mechanism("reducedS152R621_0.cti")

custom.print_fortran(mech, "ATJ", routine_name="fcmech", use="NGA")

os.system("cp YAO.f90 ../src/YAO.f90")

# fuel = {"NXC12H26":1.0}
# oxidizer = {"O2":1.0, "N2":3.76}

# soxi = tools.compute_stoichiometric_ratio(fuel, oxidizer, mech.ctmech, type="mole")

# print("Stoichiometric ratio: {}".format(1.0/soxi))