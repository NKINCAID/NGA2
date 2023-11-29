import cantera as ct
import numpy as np

import matplotlib.pyplot as plt
import plot_params
from plot_params import lw, ls, ms, mk, c

gas = ct.Solution('cti/YAO_reduced.cti')

gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 850.0, 3.4e6

r = ct.IdealGasConstPressureReactor(gas)


sim = ct.ReactorNet([r])

states = ct.SolutionArray(gas, extra='t')
states.append(r.thermo.state, t=0.0)

time = 0.0
dt = 1.0e-5

for i in range(1,100):
    sim.advance(time + dt)
    time += dt

    states.append(r.thermo.state, t=time)

    # print(i, time)
    # print('----------------------------------------------------------------------------------------------------')
    # print('{:20} | {:20} | {:20} | {:20} '.format(" ", "Old", "New", "Delta"))
    # print('----------------------------------------------------------------------------------------------------')
    # print('{:20} | {:20.7e} | {:20.7e} | {:20.7e}'.format("Temp", states.T[i-1], states.T[i], states.T[i] - states.T[i-1]))
    # specs = ["NXC12H26", "O2", "HO2", "SXC12H25"]
    # for nsc in range(gas.n_species):
    #     delta = (states[i].Y[nsc] - states[i-1].Y[nsc])
    #     if gas.species_names[nsc] in specs:
    #         print('{:20} | {:20.7e} | {:20.7e} | {:20.7e}'.format(gas.species_names[nsc], states[i-1].Y[nsc], states[i].Y[nsc], delta))
    # print(states[i].P)
    # print('----------------------------------------------------------------------------------------------------\n\n')


# print('--------------------------------------------------')
# print('{:20} | {:20} | {:20} '.format(" ", "hsp", "cp"))
# print('--------------------------------------------------')
# gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
# gas.TP = 750.0, 3.4e6
# for i, spec in enumerate(gas.species()):
#     print("{:20} | {:20.7e} | {:20.7e}".format(gas.species_names[i], spec.thermo.h(gas.T), spec.thermo.cp(gas.T)))
# print('--------------------------------------------------\n\n')


fig, ax = plt.subplots(1, 1, figsize=(8, 6))
ax.plot(
    states.t * 1000,
    states.T,
    lw=lw,
    label="Cantera",
    color=c[0],
)

ax.set(
    xlabel="Time [ms]",
    ylabel="Temperature [K]",
    ylim=[650, 3100],
)
ax.grid()

ax.legend(frameon=False, loc="lower right")

plt.title(r"Constant Pressure Reactor")

plt.tight_layout()

fig, ax = plt.subplots(1, 1, figsize=(8, 6))
ax.plot(
    states.t * 1000,
    states.density,
    lw=lw,
    label="Cantera",
    color=c[0],
)

ax.set(
    xlabel="Time [ms]",
    ylabel="Density [kg/m3]",
    # ylim=[650, 3100],
)
ax.grid()

ax.legend(frameon=False, loc="lower right")

plt.title(r"Constant Pressure Reactor")

plt.tight_layout()
plt.show()