import cantera as ct
import numpy as np
from pathlib import Path

import matplotlib.pyplot as plt
import plot_params
from plot_params import lw, ls, ms, mk, c
from matplotlib.lines import Line2D


gas = ct.Solution('cti/YAO_reduced.cti')

gas.set_equivalence_ratio(1.0, "NXC12H26", "O2:1.0,N2:3.76")
gas.TP = 300.0, 3.4e6
width = 0.03  # m
loglevel = 1  # amount of diagnostic output (0 to 8)

# Set up flame object
f = ct.FreeFlame(gas, width=width)
f.set_refine_criteria(ratio=3, slope=0.06, curve=0.12)
f.transport_model = 'multicomponent'

output = "./laminar_flame_data/flame.yaml"
# f.show()

try:
    f.restore(output, name="multi")
except:
    # Solve with multi-component transport properties
    f.solve(loglevel)  # don't use 'auto' on subsequent solves
    # f.show()

    f.save(output, name="multi", description="solution with multicomponent transport")

print(f"multicomponent flamespeed = {f.velocity[0]:7f} m/s")

Yin = f.Y[:,0]
Yb = f.Y[:,-1]

for i,spec in enumerate(gas.species_names):
    if Yin[i] > 1.0e-10: 
        print("Inflow {:10}    :    {:.7e}".format(spec, Yin[i]))
print()
for i,spec in enumerate(gas.species_names):
    if Yb[i] > 1.0e-10: 
        print("Burnt {:10}    :    {:.7e}".format(spec, Yb[i]))
# fig, ax = plt.subplots(1, 1, figsize=(8, 6))
# ax.plot(
#     states.t * 1000,
#     states.T,
#     lw=lw,
#     label="Cantera",
#     color=c[0],
# )

# ax.plot(
#     time_array  * 1000,
#     T_array,
#     lw=lw,
#     ls="--",
#     label="NGA2",
#     color=c[1],
# )

# ax.set(
#     xlabel="Time [ms]",
#     ylabel="Temperature [K]",
#     ylim=[650, 3100],
# )
# ax.grid()

# ax.legend(frameon=False, loc="lower right")

# plt.title(r"Constant Volume Reactor")

# plt.tight_layout()
# # plt.savefig("temperature.png")
