**README.md**

The file '_data load module.R_' creates a subfolder in the data folder (e.g., 'akfin') that is meant to be specific to the user ('jraymond' in my case). The working directory is set to this folder so that all temporary files never leave the parent data folder. 

The code can then read all the files in the data folder that end in ".csv". Each file is stored in a data frame which can then be saved in the subfolder to work on.