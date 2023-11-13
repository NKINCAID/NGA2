import cantera as ct
import numpy as np

gas = ct.Solution('cti/fake.cti')

gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 750.0, 3.4e6

r = ct.IdealGasConstPressureReactor(gas)


sim = ct.ReactorNet([r])

states = ct.SolutionArray(gas)
states.append(r.thermo.state)

sim.advance(1.0e-5)

states.append(r.thermo.state)


print('--------------------------------------------------')
print('{:10} | {:10} | {:10} | {:10} '.format(" ", "Old", "New", "Delta"))
print('--------------------------------------------------')
print('{:10} | {:10.2f} | {:10.2f} | {:10.2f}'.format("Temp", states.T[0], states.T[1], states.T[1] - states.T[0]))

for i in range(gas.n_species):
    delta = abs(states[1].Y[i] - states[0].Y[i])
    if delta > 1e-8:
        print('{:10} | {:10.4e} | {:10.4e} | {:10.2e}'.format(gas.species_names[i], states[0].Y[i], states[1].Y[i], delta))
print('--------------------------------------------------\n\n')


print('--------------------------------------------------')
print('{:10} | {:10} | {:10} '.format(" ", "hsp", "cp"))
print('--------------------------------------------------')
gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 750.0, 3.4e6
for i, spec in enumerate(gas.species()):
    print("{:10} | {:10.2e} | {:10.2e}".format(gas.species_names[i], spec.thermo.h(gas.T), spec.thermo.cp(gas.T)))
print('--------------------------------------------------\n\n')

print('--------------------------------------------------')
gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 750.0, 3.4e6
print("{:10} | {:10.2e}".format("Density", gas.density))
print("{:10} | {:10.2e}".format("Viscosity", gas.viscosity))

print('--------------------------------------------------\n\n')