import cantera as ct
import numpy as np
import matplotlib.pyplot as plt
import time as timer


def simulate_ignition(gas, T0, P, fuel, phi, return_states=False):
    gas.TP = T0, P
    gas.set_equivalence_ratio(phi, fuel, "O2:1.0, N2:3.76")
    print(gas.enthalpy_mass)

    r = ct.IdealGasConstPressureReactor(gas)

    sim = ct.ReactorNet([r])

    time = 0.0
    tend = 1.0

    states = ct.SolutionArray(gas, extra=["t", "HR"])
    ignition = False
    while time < tend:
        time = sim.step()
        # time += 1.0e-5
        # sim.advance(time)
        # print(gas.enthalpy_mass)
        hr = -np.dot(r.thermo.net_production_rates, r.thermo.partial_molar_enthalpies)
        states.append(r.thermo.state, t=time * 1000, HR=hr)

        if r.thermo.T >= T0 + 500 and ignition == False:
            tend = time * 1.5
            tig1 = time * 1000
            ignition = True
    print(gas.enthalpy_mass)
    ind_ig = np.argmax(states.HR)
    tig = states.t[ind_ig]
    print("Ignition at: {:.4f} ms".format(tig))

    if return_states:
        return tig, states
    else:
        return tig


t1 = timer.time()

octane_mech = ct.Solution("cti/simple_mech.cti")

P = 1.0e6
phi = 1.0

plot_profile = True


# temps = np.arange(750, 1300, 10)
temps = np.array([750])
states_array = []
tig_array = []

for T in temps:
    tig, states = simulate_ignition(
        gas=octane_mech, T0=T, P=P, fuel="IC8H16", phi=phi, return_states=True
    )
    tig_array.append(tig)
    states_array.append(states)

t2 = timer.time()

print("Elapsed time: {:.4f}".format(t2 - t1))


lw = 2.5
ms = 4

c = [
    "#e41a1c",
    "#377eb8",
    "#4daf4a",
    "#984ea3",
    "#ff7f00",
    "#ffff33",
    "#a65628",
    "#f781bf",
]
ls = ["-", "--", "-.", ":"]

plt.rcParams.update({"font.size": 20})
plt.rcParams["mathtext.fontset"] = "stix"
plt.rcParams["font.family"] = "Times New Roman"
lims = [[-1, 1], [-0.2, 0.2]]
if plot_profile:
    fig, ax = plt.subplots(1, 1, figsize=(9, 6))
    for i, states in enumerate(states_array):
        ax.plot(
            states.t,
            states.T,
            color=c[i],
            lw=lw,
            ls=ls[i],
            # marker='o',
            # markersize=ms,
            # markerfacecolor='none',
            # markevery=10,
            label=r"Octane, $T_0%$={:.0f} K".format(temps[i]),
        )
        ax.set(
            xlabel=r"$t [\mathrm{ms}]$",
            ylabel="Temperature [K]",
            # xlim=lims[i],
            # xscale='log'
        )
        ax.legend(frameon=False, loc="upper left")
        ax.grid(axis="y", which="major")

    plt.tight_layout()
    # plt.savefig("figs/octane-det-profile.png")
    plt.show()
