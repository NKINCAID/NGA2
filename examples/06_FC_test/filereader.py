import struct
import numpy as np

# File name
filename = 'IC_CP0D_ATJ'

# Read the binary file
with open(filename, 'rb') as f:

    # Read pressure
    pressure_bytes = f.read(8)
    pressure = struct.unpack('d', pressure_bytes)[0]
    print('Pressure = ', pressure)

    # Read number of data sets
    num_data_sets = struct.unpack('i', f.read(4))[0]
    print('Number of ICs = ', num_data_sets)

    # Read number of species
    nspec = struct.unpack('i', f.read(4))[0]
    print('Number of species = ', nspec)

    # Read species names
    species_names = []
    for _ in range(nspec):
        name_length = struct.unpack('i', f.read(4))[0]
        spname = f.read(name_length).decode('utf-8')
        species_names.append(spname)

    # Read the data
    IC = np.zeros((nspec+1, num_data_sets))
    for idata in range(num_data_sets):
        for isc in range(nspec+1):
            IC_bytes = f.read(4)
            IC[isc, idata] = struct.unpack('f', IC_bytes)[0]


# Test
print(IC.shape)

realization = 0
print('######## Realization ', realization)
print('Mass fractions:')
for _ in range(nspec):
    print(species_names[_], ' = ', IC[_, realization])
print('Temperazture = ', IC[nspec, realization], ' K')

realization = 1
print('######## Realization ', realization)
print('Mass fractions:')
for _ in range(nspec):
    print(species_names[_], ' = ', IC[_, realization])
print('Temperazture = ', IC[nspec, realization], ' K')