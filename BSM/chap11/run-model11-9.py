import pandas
import cmdstanpy

d = pandas.read_csv('input/data-pulse.csv')
T = len(d)
data = {'T':T, 'Y':d.Y}

model = cmdstanpy.CmdStanModel(stan_file='model/model11-9.stan')
fit = model.sample(data=data, seed=123, parallel_chains=4)
fit.save_csvfiles('output/result-model11-9')
