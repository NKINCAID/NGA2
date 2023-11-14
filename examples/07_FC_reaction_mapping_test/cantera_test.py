import cantera as ct
import numpy as np

gas = ct.Solution('cti/YAO_reduced.cti')

gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 750.0, 3.4e6

r = ct.IdealGasConstPressureReactor(gas)


sim = ct.ReactorNet([r])

states = ct.SolutionArray(gas)
states.append(r.thermo.state)

sim.advance(1.0e-5)

states.append(r.thermo.state)


print('----------------------------------------------------------------------------------------------------')
print('{:20} | {:20} | {:20} | {:20} '.format(" ", "Old", "New", "Delta"))
print('----------------------------------------------------------------------------------------------------')
print('{:20} | {:20.7e} | {:20.7e} | {:20.7e}'.format("Temp", states.T[0], states.T[1], states.T[1] - states.T[0]))
specs = ["NXC12H26", "O2", "HO2", "SXC12H25"]
for i in range(gas.n_species):
    delta = abs(states[1].Y[i] - states[0].Y[i])
    if gas.species_names[i] in specs:
        print('{:20} | {:20.7e} | {:20.7e} | {:20.7e}'.format(gas.species_names[i], states[0].Y[i], states[1].Y[i], delta))
print('----------------------------------------------------------------------------------------------------\n\n')


# print('--------------------------------------------------')
# print('{:20} | {:20} | {:20} '.format(" ", "hsp", "cp"))
# print('--------------------------------------------------')
# gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
# gas.TP = 750.0, 3.4e6
# for i, spec in enumerate(gas.species()):
#     print("{:20} | {:20.7e} | {:20.7e}".format(gas.species_names[i], spec.thermo.h(gas.T), spec.thermo.cp(gas.T)))
# print('--------------------------------------------------\n\n')

print('--------------------------------------------------')
gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 750.0, 3.4e6
print("{:20} | {:20.3e}".format("Density", gas.density))
print("{:20} | {:20.3e}".format("Viscosity", gas.viscosity))

print('--------------------------------------------------\n\n')