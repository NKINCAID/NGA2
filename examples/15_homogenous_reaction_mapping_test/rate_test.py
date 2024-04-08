import cantera as ct
import numpy as np


import matplotlib.pyplot as plt
import plot_params
from plot_params import lw, ls, ms, mk, c
from matplotlib.lines import Line2D

ct.suppress_thermo_warnings()

import cantera as ct
import numpy as np


import matplotlib.pyplot as plt
import plot_params
from plot_params import lw, ls, ms, mk, c
from matplotlib.lines import Line2D



# file =  "monitor/simulation"
# with open(file, 'r') as f:
#     lines = f.readlines()

# names = lines[0].split()
# lines.pop(0)
# lines.pop(0)

# data = []
# for line in lines:
#     split_line = line.split()
#     data.append([float(val) for val in split_line])
# data = np.array(data)

# time_array = data[:,names.index('Time')]
# T_array = data[:,names.index('Tmax')]
# P_array = data[:,names.index("Pthermo")]

# gas = ct.Solution('cti/YAO_reduced.cti')
# gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")

gas = ct.Solution('cti/reducedS152R621_0.cti')
gas.set_equivalence_ratio(1.0, "XC12H26:0.8649, HMN:0.1351", "O2:1.0,N2:3.76")

gas.TP = 1000.0, 3.4e6



r = ct.IdealGasConstPressureReactor(gas)


sim = ct.ReactorNet([r])

states = ct.SolutionArray(gas, extra='t')
states.append(r.thermo.state, t=0.0)

time = 0.0
dt = 1.0e-6

for i in range(1,550):
    sim.advance(time + dt)
    time += dt

    states.append(r.thermo.state, t=time)



print()
print("Initial {:10}    :    {:.7e}".format("enthalpy", gas.enthalpy_mass))
print("Initial {:10}    :    {:.7e}".format("temperature", gas.T))

for i,spec in enumerate(gas.species_names):
    if "(" in spec:
        spec = spec.replace("(", "G")
    if ")" in spec:
        spec = spec.replace(")", "G")
    if "-" in spec:
        spec = spec.replace('-', "X")

    print("Initial {:20}    :    {:.16e}".format(spec, gas.Y[i]))
print()


# plt.savefig("f.png")


print("---------------------------------------------------------------------------------------------------------------------------------------")
print("|{:^9}|{:^9}|{:^15}|{:^15}|{:^15}|{:^15}|{:^60}|".format("Index","Type","Forward K", "Reverse K", "Forward Rate", "Reverse Rate", "Equation"))
print("---------------------------------------------------------------------------------------------------------------------------------------")

for i in range(gas.n_reactions):
    print("|{:9}|{:9}|{:15.7e}|{:15.7e}|{:15.7e}|{:15.7e}|{:60}|".format(i+1,gas.reaction_type(i), gas.forward_rate_constants[i], gas.reverse_rate_constants[i], gas.forward_rates_of_progress[i], gas.reverse_rates_of_progress[i],gas.reaction_equations()[i]))
    print("---------------------------------------------------------------------------------------------------------------------------------------")

print()
print()
print("----------------------------------")

print("|{:^9}|{:^15}|{:^15}|".format("Spec", "Y[i]", "Conc[i]"))
print("----------------------------------")

for i in range(gas.n_species):
    print("|{:9}|{:15.7e}|{:15.7e}|".format(i+1, gas.Y[i], gas.concentrations[i] ))
    print("----------------------------------")
print()
print()
