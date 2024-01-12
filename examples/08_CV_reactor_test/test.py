import cantera as ct
import numpy as np
import matplotlib.pyplot as plt
import time as timer


def simulate_ignition(gas, T0, P, fuel, phi, return_states=False):
    gas.TP = T0, P
    gas.set_equivalence_ratio(phi, fuel, "O2:1.0, N2:3.76")

    r = ct.IdealGasReactor(gas)

    sim = ct.ReactorNet([r])

    time = 0.0
    tend = 1.0

    states = ct.SolutionArray(gas, extra=["t", "HR"])

    ignition = False
    while time < tend:
        time = sim.step()
        # time += 1.0e-5
        # sim.advance(time)

        hr = -np.dot(r.thermo.net_production_rates, r.thermo.partial_molar_enthalpies)
        states.append(r.thermo.state, t=time * 1000, HR=hr)

        if r.thermo.T >= T0 + 500 and ignition == False:
            tend = time * 1.2
            tig1 = time * 1000
            ignition = True

    ind_ig = np.argmax(states.HR)
    tig = states.t[ind_ig]
    print("Ignition at: {:.4f} ms".format(tig))

    if return_states:
        return tig, states
    else:
        return tig


heptane_mech = ct.Solution("cti/YAO_reduced.cti")
octane_mech = ct.Solution("cti/reducedS152R621_0.cti")
P = 3.4e6
phi = 1.0

plot_profile = False
plot_tig = True

temps = np.arange(650, 1300, 5)
# temps = np.array([700, 1300])
# temps = np.array([750])
tig_heptane = []
tig_octane = []
for T in temps:
    t1 = timer.time()
    tig_heptane.append(
        simulate_ignition(gas=heptane_mech, T0=T, P=P, fuel="NXC12H26", phi=phi)
    )
    t2 = timer.time()
    tig_octane.append(
        simulate_ignition(gas=octane_mech, T0=T, P=P, fuel="XC12H26:0.8649, HMN:0.1351", phi=phi)
    )
    t3 = timer.time()
    print("Heptane runtime: {:.4f}".format(t2 - t1))
    print("Octane runtime: {:.4f}".format(t3 - t2))
lw = 2.5
ms = 8

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
# if plot_profile:
#     fig, axs = plt.subplots(2, 2, figsize=(8, 6))
#     ax = axs[0, 0]
#     ax.plot(states.t, states.T)
#     ax.set(xlabel="Time (ms)", ylabel="Temperature (K)")

#     ax = axs[0, 1]
#     ax.plot(states.t, states.Y[:, gas.species_index("OH")])
#     ax.set(xlabel="Time (ms)", ylabel="OH Mass Fraction")

#     ax = axs[1, 0]
#     ax.plot(states.t, states.Y[:, gas.species_index("H")])
#     ax.set(xlabel="Time (ms)", ylabel="H Mass Fraction")

#     ax = axs[1, 1]
#     ax.plot(states.t, states.Y[:, gas.species_index(fuel)])
#     ax.set(xlabel="Time (ms)", ylabel="{} Mass Fraction".format(fuel))

#     plt.tight_layout()
#     plt.show()

if plot_tig:


    fig, ax = plt.subplots(1, 1, figsize=(10, 6))
    ax.plot(
        1000.0 / temps,
        tig_heptane,
        color=c[0],
        lw=lw,
        label="Dodecane - YAO",
    )
    ax.plot(
        1000.0 / temps,
        tig_octane,
        color=c[1],
        lw=lw,
        label="ATJ",
    )
    ax.set(
        xlabel=r"$\frac{1000 \mathrm{K}}{T}$",
        ylabel=r"$\tau\ [\mathrm{ms}]$",
        yscale="log",
    )
    ax.legend(frameon=False, prop={'size': 18}, loc='upper left')
    ax.grid(axis='y',which='major')

    plt.tight_layout()
    plt.savefig('figs/tig_vs_T.png')
    plt.show()
