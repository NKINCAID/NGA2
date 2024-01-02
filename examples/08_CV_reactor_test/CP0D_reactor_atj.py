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

gas = ct.Solution('cti/YAO_reduced.cti')
gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")

# gas = ct.Solution('cti/reducedS152R621_0.cti')
# gas.set_equivalence_ratio(1.0, "XC12H26:0.8649, HMN:0.1351", "O2:1.0,N2:3.76")

gas.TP = 750.0, 3.4e6

print()
print("Initial {:10}    :    {:.7e}".format("enthalpy", gas.enthalpy_mass))
for i,spec in enumerate(gas.species_names):
    if gas.Y[i] > 1.0e-10: 
        print("Initial {:10}    :    {:.7e}".format(spec, gas.Y[i]))
print()


r = ct.IdealGasConstPressureReactor(gas)


sim = ct.ReactorNet([r])

states = ct.SolutionArray(gas, extra='t')
states.append(r.thermo.state, t=0.0)

time = 0.0
dt = 1.0e-6

for i in range(1,10000):
    sim.advance(time + dt)
    time += dt

    states.append(r.thermo.state, t=time)

fig, ax = plt.subplots(1, 1, figsize=(8, 6))
ax.plot(
    states.t * 1000,
    states.T,
    lw=lw,
    label="Cantera",
    color=c[0],
)

# ax.plot(
#     time_array  * 1000,
#     T_array,
#     lw=lw,
#     ls="--",
#     label="NGA2",
#     color=c[1],
# )

ax.set(
    xlabel="Time [ms]",
    ylabel="Temperature [K]",
    ylim=[650, 3100],
)
ax.grid()

ax.legend(frameon=False, loc="lower right")

plt.title(r"Constant Volume Reactor")

plt.tight_layout()
plt.savefig("temperature.png")

plt.show()
# plt.savefig("f.png")
