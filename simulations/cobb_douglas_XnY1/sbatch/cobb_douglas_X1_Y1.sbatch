#!/bin/bash
#SBATCH --job-name=V01_200     	 	# Nombre del trabajo
#SBATCH --ntasks=1				        # Una única tarea
#SBATCH --cpus-per-task=1			    # Cores solicitados
#SBATCH --mem-per-cpu=4096		        # Memoria solicitada MB/core
#SBATCH --output=/home/PI/ricardo.gonzalezm/cafee/nX_01/%j.log	    # Fichero de salida
#SBATCH --error=/home/PI/ricardo.gonzalezm/cafee/nX_01/%j.err		# Fichero de error
#SBATCH --mail-user=ricardo.gonzalezm@umh.es		# Email donde enviar eventos
#SBATCH --mail-type=BEGIN,END,FAIL	    # Eventos a enviar por email
#SBATCH --time=10-0						# Días de duración: 10 días

export OMP_NUM_THREADS=${SLURM_CPUS_ON_NODE}
 
module load R/4.2.2
cd /home/PI/ricardo.gonzalezm/cafee/nX_01

#Rscript cobb_douglas_XnY1_01_025.R
#Rscript cobb_douglas_XnY1_01_050.R
#Rscript cobb_douglas_XnY1_01_150.R
Rscript cobb_douglas_XnY1_01_200.R