import cantera as ct
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D


file_sch_of =  "monitor/simulation_scheduling_off"
with open(file_sch_of, 'r') as f:
    lines_of = f.readlines()

names_of = lines_of[0].split()
lines_of.pop(0)
lines_of.pop(0)

data_of = []
for line in lines_of:
    split_line = line.split()
    data_of.append([float(val) for val in split_line])
data_of = np.array(data_of)
time_array_of = data_of[:,names_of.index('Time')]
T_array_of = data_of[:,names_of.index('Temperature')]

file_sch_on =  "monitor/simulation_scheduling_on"
with open(file_sch_on, 'r') as f:
    lines_on = f.readlines()

names_on = lines_on[0].split()
lines_on.pop(0)
lines_on.pop(0)

data_on = []
for line in lines_on:
    split_line = line.split()
    data_on.append([float(val) for val in split_line])
data_on = np.array(data_on)
time_array_on = data_on[:,names_on.index('Time')]
T_array_on = data_on[:,names_on.index('Temperature')]

gas = ct.Solution('cti/YAO_reduced.cti')
gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 1000.0, 3.4e6

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

for i in range(1,1000):
    sim.advance(time + dt)
    time += dt

    states.append(r.thermo.state, t=time)

fig, ax = plt.subplots(1, 1, figsize=(8, 6))
ax.plot(
    states.t * 1000,
    states.T,
    lw=2.5,
    label="Cantera",
    color='black',
)

ax.plot(
    time_array_of  * 1000,
    T_array_of,
    lw=2.5,
    label="NGA2 Scheduling off",
    color='blue',
)

ax.plot(
    time_array_on  * 1000,
    T_array_on,
    lw=2.5,
    ls="--",
    label="NGA2 Scheduling on",
    color='red',
)

ax.set(
    xlabel="Time [ms]",
    ylabel="Temperature [K]",
    ylim=[650, 3100],
)
ax.grid()
ax.legend(frameon=False, loc="lower right")\

plt.title(r"Constant Pressure Reactor")
plt.tight_layout()
plt.savefig("Temperature.png")
plt.show()