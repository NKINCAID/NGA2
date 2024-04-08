import cantera as ct
import numpy as np
from pathlib import Path

import matplotlib.pyplot as plt
import plot_params
from plot_params import lw, ls, ms, mk, c
from matplotlib.lines import Line2D


# gas = ct.Solution('cti/YAO_reduced.cti')
# gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")


gas = ct.Solution('cti/reducedS152R621_0.cti')
gas.set_equivalence_ratio(1.0, "XC12H26:0.8649, HMN:0.1351", "O2:1.0,N2:3.76")

gas.TP = 900.0, 3.4e6
width = 3e-4  # m
loglevel = 1  # amount of diagnostic output (0 to 8)

restore = False

# Set up flame object
f = ct.FreeFlame(gas, width=width)
f.set_refine_criteria(ratio=3, slope=0.06, curve=0.12)
# f.transport_model = 'multicomponent'

output = "./laminar_flame_data/flame.yaml"
# f.show()

if restore:
    try:
        f.restore(output, name="multi")
    except:
        print("Couldn't restore...")
else:
    # Solve with multi-component transport properties
    f.solve(loglevel)  # don't use 'auto' on subsequent solves
    # f.show()

    f.save(output, name="multi", description="solution with multicomponent transport")

print(f"multicomponent flamespeed = {f.u[0]:7f} m/s")

Yin = f.Y[:,0]
Yb = f.Y[:,-1]

for i,spec in enumerate(gas.species_names):
    if Yin[i] > 1.0e-10: 
        print("Inflow {:10}    :    {:.7e}".format(spec, Yin[i]))
print()
for i,spec in enumerate(gas.species_names):
    if Yb[i] > 1.0e-10: 
        print("Burnt {:10}    :    {:.7e}".format(spec, Yb[i]))


#compute flame thickness
z= f.flame.grid
T = f.T
size = len(z)-1
grad = np.zeros(size)
for i in range(size):
  grad[i] = (T[i+1]-T[i])/(z[i+1]-z[i])
thickness = (max(T) -min(T)) / max(grad)
location = f.grid[np.argmax(grad)]

print('laminar flame thickness = ', thickness)
print('laminar flame location = ', location)



fig, ax = plt.subplots(1, 1, figsize=(10, 6))
ax.plot(
    f.grid * 1000,
    f.T,
    lw=lw,
    label="Temperature",
    color=c[0],
)


ax.set(
    xlabel="Grid [mm]",
    ylabel="Temperature [K]",
    ylim=[500, 3000],
)
ax2 = ax.twinx()

ax2.plot(
    f.grid * 1000,
    f.u,
    lw=lw,
    label="Velocity",
    color=c[1],
)


ax2.set(
    ylabel="Velocity [m/s]",
    ylim=[0, 2.0],
)

ax.grid()

ax.legend(frameon=False, loc="upper left")
ax2.legend(frameon=False, loc="lower right")


plt.title(r"1D Laminar Flame")

plt.tight_layout()
plt.show()
# plt.savefig("temperature.png")
