import os

import pandas  as pd
import numpy   as np
# import seaborn as sns

import matplotlib.pyplot as plt


import utils


colors_strat = plt.rcParams['axes.prop_cycle'].by_key()['color']

dict_colors = \
	{  'quantile':      colors_strat[1],
	   'linear regression': colors_strat[3], 'predict_linear':colors_strat[3],
 	   'random forest': 	colors_strat[4], 'predict_rf': 	  colors_strat[4],
 	   'lin + RF': colors_strat[6]
	   }
dict_colors.update({'return_'+str(_length)+'yr': colors_strat[_length-5]
					for _length in [5, 7, 10, 12]})
dict_colors.update({'return_'+str(_length)+'yr': colors_strat[_length-1] for _length in [1, 2, 3]})
dict_colors['return_15yr'] 		= colors_strat[1]
dict_colors['return_20yr'] 		= colors_strat[1]
dict_colors['return_avg_1_7yrs']= colors_strat[8]
dict_colors['return_avg_9_15yrs']=colors_strat[3]
dict_colors['TR_min'] 	   		= colors_strat[9]
dict_colors['inv_TR_ratio_1yr']	= colors_strat[9]
# dict_colors = \
# 	{  'linear regression': colors_strat[0], 'predict_linear':colors_strat[0],
# 	   'random forest': 	colors_strat[1], 'predict_rf': 	  colors_strat[1],
# 	   'drawdowns':    		colors_strat[2], 'DD':    		  colors_strat[2],
# 	   'meta3':    			colors_strat[3], 'quantile':      colors_strat[3],
# 	   'min': 				colors_strat[4],
# 	   'max': 		  		colors_strat[5],
# 	   'SP500': 			colors_strat[6], 'actual': 		  colors_strat[6],
# 	   'constant': 			colors_strat[7],
# 	   'future returns': 	colors_strat[8], 'meta2': 		  colors_strat[8],
# 	   			'lin + RF': colors_strat[8],
# 	   'avg': 				colors_strat[9]}
# print('colors:', dict_colors)


months_SMA_plots 	= 3
alpha_all    		= 0.25
alpha_Pareto 		= 1.

_range_max_loss 	= ( 0.,   85.)
_range_avg_loss 	= ( 0.,   11.)
_range_volatility 	= ( 0.,   22.)
_range_vol_10yr		= ( 0.,   27.)
_range_allocation 	= (-0.5, 100.5)
_range_score	 	= (-1.5,   3.)

utils.months_SMA_plots = months_SMA_plots

# def color_from_source(name, dict_colors=dict_colors):
# 	if name in dict_colors: return dict_colors[name]
# 	for _

# PREDICTIONS
#############

# scatter: prediction as function of actual
def actual_predicted(actual, predict_df, label_y, name_range):
	for _name in predict_df.columns:
		plt.scatter(100. * actual, 100. * predict_df[_name], c=dict_colors[_name], label=_name)

	my_min = 100. * min(min(actual), min([min(predict_df[_name]) for _name in predict_df.columns])) - 2
	my_max = 100. * max(max(actual), max([max(predict_df[_name]) for _name in predict_df.columns])) + 2
	# print(my_min, my_max)
	plt.axis('square');  plt.xlim(my_min, my_max);  plt.ylim(my_min, my_max)

	plt.legend()
	plt.xlabel('actual [% p.a.]');  plt.ylabel('predicted [% p.a.]')
	plt.title(label_y + (' ('+name_range+')' if name_range is not None else ''))
	plt.axline((0, 0), slope=1)
	_fname = "return-actual-predicted" + \
			 ('-'+name_range if name_range is not None else '') + ".png"
	plt.savefig(os.path.join('figures', _fname), format="png", dpi=200)
	plt.close()

# scatter: prediction-actual as function of actual
def actual_residual(actual, residual_df, label_y, name_range, range_residual):
	for _name in residual_df.columns:
		plt.scatter(100. * actual, 100. * residual_df[_name], c=dict_colors[_name], label=_name)

	my_min = 100. * min(actual.min(), range_residual[0]) - 2
	my_max = 100. * max(actual.max(), range_residual[1]) + 2
	plt.axis('square');  plt.xlim(my_min, my_max)
	plt.ylim(-(my_max-my_min)/2., (my_max-my_min)/2.)
	plt.hlines(0, my_min, my_max, colors='k')

	plt.legend()
	plt.xlabel('actual [% p.a.]'); plt.ylabel('predicted - actual [% p.a.]')
	plt.title(label_y + (' ('+name_range+')' if name_range is not None else ''))
	_fname = "return-actual-residual" + \
			 ('-'+name_range if name_range is not None else '') + ".png"
	plt.savefig(os.path.join('figures', _fname), format="png", dpi=200)
	plt.close()


# DISTRIBUTIONS
###############

def cumul_dist(dict_returns, name_range, label_y, nfile=None,
			   xlim=None, ylim=None, log=(False, False), occurrences_SMA=30):
	if nfile is None: nfile=label_y

	for _name_curve in dict_returns:
		_returns = (100*pd.Series(dict_returns[_name_curve][name_range])).\
			rolling(window=occurrences_SMA, center=True).mean().dropna().to_list()
		# print(_name_curve, _returns)
		_length = len(_returns)
		plt.plot([e for e in _returns],
				 [100 * (1 - e/_length) for e in range(_length)], label=_name_curve)

	plt.title('cumulative '+label_y);	plt.legend(loc='upper right')
	plt.xlabel(label_y + ' [% p.a.]')

	if log[1]:
		# plt.ylim(0.01, 100)
		if log[0]:
			plt.loglog()
		else:
			plt.semilogy()
	else:
		# plt.ylim(0, 100)
		if log[0]:
			plt.semilogx()

	plt.xlim(xlim)
	plt.ylim(ylim)

	plt.savefig(os.path.join('figures', "cumul_dist-" + nfile + "-" + name_range + ".png"),
				format="png", dpi=200)
	plt.close()



# TIME EVOLUTION
################

# capital (`TR`) as function of time
def time_capital(df_TR, label_y, nfile=None, months_SMA=months_SMA_plots):
	if nfile is None: nfile=label_y
	_df_TR = df_TR.rolling(window=months_SMA, center=True).mean()

	for _name_curve in _df_TR.columns:
		plt.plot(_df_TR.index, _df_TR[_name_curve], # c=dict_colors[_name_curve],
				 label=_name_curve)

	if "train" in label_y:
		plt.ylim(0.8, 600)
	elif "valid" in label_y:
		plt.ylim(0.6, 100)
	elif "recent" in label_y:
		plt.ylim(0.9,  20)

	plt.title(label_y);	plt.legend(loc='upper left')
	plt.semilogy();  plt.ylabel('capital')
	plt.savefig(os.path.join('figures', "time-capital-" + nfile + ".png"), format="png", dpi=200)
	plt.close()


# returns as function of time
def time_return(df_return, length, label_y, nfile=None,
				crop=0, ylim=(-5, 25), months_SMA=months_SMA_plots*2, do_plot=True):
	_df = df_return[[e for e in df_return.columns if 'alloc' not in e]]
	_next_max = _df.fillna(-np.inf).apply(lambda row: row.nlargest(2).values[-1], axis=1)
	_return_gain = pd.Series([(_df[_name] - _next_max).clip(lower=0).mean()
							   for _name in _df.columns], index=_df.columns)

	if do_plot:
		if nfile is None: nfile = label_y
		_df_return = df_return.rolling(window=months_SMA, center=True).mean()
		for _name_curve in _df_return.columns:
			plt.plot(_df_return.index[int(crop*12):],
					 100 * _df_return[_name_curve].iloc[int(crop*12):], label=_name_curve)
		plt.title(label_y); 	plt.legend(loc='upper left')
		plt.ylabel('return over ' + str(length) + ' years [% p.a.]');  plt.ylim(ylim)
		plt.savefig(os.path.join('figures', "time-return_"+str(length)+'yr-' + nfile + ".png"),
					format="png", dpi=200)
		plt.close()

	return _return_gain


# TR residual as function of time
def time_deltaTR(df_TR, label_y):
	my_min = 100. * min(np.min(df_TR))
	my_max = 100. * max(np.max(df_TR))

	delta_TR = pd.DataFrame(pd.concat(
		[pd.concat([df_TR[[_name]], -df_TR[['actual']]], axis=1).sum(axis=1, skipna=False)
			 for _name in df_TR.columns if _name != 'actual'], axis=1))
	delta_TR.columns = [_name for _name in df_TR.columns if _name != 'actual']
	(100. * delta_TR).plot()
	plt.hlines(0., min(df_TR.index), max(df_TR.index), colors='r')
	plt.title(label_y + ': predicted - actual [% p.a.]');
	plt.xlim(start_year, 2025)
	plt.ylim(-(my_max - my_min) / 2, (my_max - my_min) / 2)
	plt.savefig(os.path.join('figures', utils.name_png('meta') + "-time-delta_TR.png"),
				format="png", dpi=200)
	plt.close()


def time_allocations(allocations, label_y, nfile=None, months_SMA=int(months_SMA_plots/2)):
	if nfile is None: nfile=label_y
	# _range_years = [min(range_years), max(range_years)]
	# print('range_years:', range_years)
	# print('_range_years:', _range_years)
	# allocations = allocations[allocations.index >= _range_years[0]-months_SMA/12/2.]
	# allocations = allocations[allocations.index <= _range_years[1]+months_SMA/12/2.] * 100.
	_allocations = allocations.rolling(window=months_SMA, center=True).mean() * 100.

	for _name_curve in _allocations.columns:
		# _color = dict_colors[_name_curve]
		plt.plot(_allocations.index, _allocations[_name_curve], # c=_color,
				 label=_name_curve)
		# plt.hlines(allocations[_name_curve].mean(), _range_years[0], _range_years[1]) # , colors=_color)
	# _df.plot()
	plt.ylim(-0.5, 100.5);  # plt.xlim(_range_years)
	plt.ylabel('allocation [%]');  plt.legend(loc='upper left');  plt.title(label_y)
	plt.savefig(os.path.join('figures', 'time-alloc-'+nfile+'.png'), format="png", dpi=200)
	plt.close()


def time_stdev_allocations(allocations, label_y, nfile=None, months_SMA=months_SMA_plots):
	if nfile is None: nfile=label_y
	_stdev = allocations * 100.
	if allocations.shape[1] > 1:   _stdev = _stdev.std(axis=1)
	_stdev = _stdev.rolling(window=months_SMA, center=True).mean()

	_stdev.plot()
	plt.ylim(-0.05, 20.)
	plt.ylabel('std. deviation allocations [%]');  plt.title(label_y)
	plt.savefig(os.path.join('figures', 'time-stdev_alloc-'+nfile+'.png'), format="png", dpi=200)
	plt.close()


def time_slope_allocations(allocations, label_y, nfile=None,
						   months_delay=1, months_SMA=int(months_SMA_plots/2)):
	if nfile is None: nfile=label_y
	_slope = ((allocations - allocations.shift(months_delay))/months_delay*12).\
				 rolling(window=months_SMA, center=True).mean() * 100.

	for _name_curve in _slope.columns:
		plt.plot(_slope.index, _slope[_name_curve], label=_name_curve)
	plt.plot(_slope.index, _slope.mean(axis=1), label="average")

	plt.ylim(-70.2, 70.2);  plt.legend();  # loc='upper left');
	plt.ylabel('slope allocation [% / year]');  plt.title(label_y)
	plt.savefig(os.path.join('figures', 'time-slope_alloc-'+nfile+'.png'), format="png", dpi=200)
	plt.close()



# SCATTER FOR STRATEGIES
########################

def vol_return(x, x_Pareto, x_curve, y, y_Pareto, y_curve, label_x, label_y, 
			   nfile, xlim=None, ylim=None, name_range=None, legend_loc=None, 
			   square=False, alpha=alpha_all, alpha_Pareto=alpha_Pareto,
			   dict_sources=None):
	def legend_without_duplicate_labels(ax):
		handles, labels = ax.get_legend_handles_labels()
		unique = [(h, l) for i, (h, l) in enumerate(zip(handles, labels)) if l not in labels[:i]]
		ax.legend(*zip(*unique), loc=legend_loc)

	# print(x);  print(100.*x_curve);  print(y);  print(y_curve)


	if x is not None and y is not None:
		assert set(x.index) == set(y.index), 'in {0:s} not in {1:s}: {2:}, \nin {1:s} not in {0:s}: {3:}'.\
			format(label_x, label_y, {e for e in x.index if e not in y.index},
				   {e for e in y.index if e not in x.index})
		assert list(x.index) == list(y.index), '{} !=\n{}'.format(list(x.index), list(y.index))

	if x_Pareto is not None and y_Pareto is not None:
		assert list(x_Pareto.index) == list(y_Pareto.index), '{} !=\n{}'. \
			format(list(x_Pareto.index), list(y_Pareto.index))
		assert list(x_Pareto.index) == list(y_Pareto.index), '{} !=\n{}'. \
			format(list(x_Pareto.index), list(y_Pareto.index))


	# print('Plotting', nfile, name_range)
	fig, ax = plt.subplots()
	for _name_source in list_trimmed_sources: # [trimmed_name(e) for e in list_sources]:

		_list_indices = [e for e in x.index if utils.trimmed_name(dict_sources[e]) == _name_source]
		# print(_name_source, _list_indices)
		ax.scatter(100.*x.loc[_list_indices], 100.*y.loc[_list_indices],
				   c=dict_colors[_name_source], alpha=alpha, label=_name_source) # alpha: 1 opaque

		_list_indices = [e for e in x_Pareto.index if utils.trimmed_name(dict_sources[e]) == _name_source]
		ax.scatter(100.*x_Pareto.loc[_list_indices], 100.*y_Pareto.loc[_list_indices],
				   c=dict_colors[_name_source], alpha=alpha_Pareto)

	# for _name in {trimmed_name(e) for e in list_sources}:
	# 	_trimmed_name = trimmed_name(_name)
	# 	if x is not None and y is not None:
	# 		_list = [_idx for _idx in x.index
	# 				 if trimmed_name(dict_sources[_idx]) == _trimmed_name]
	# 		if len(_list) > 0:
	# 			plt.scatter(100.*x.loc[_list], 100.*y.loc[_list], c=dict_colors[_trimmed_name],
	# 						alpha=alpha, label=_trimmed_name) # alpha: float (0 transparent, 1 opaque)
	#
	# 	if x_Pareto is not None and y_Pareto is not None:
	# 		_list = [_idx for _idx in x_Pareto.index
	# 				 if trimmed_name(dict_sources[_idx]) == _trimmed_name]
	# 		if len(_list) > 0:
	# 			plt.scatter(100.*x_Pareto.loc[_list], 100.*y_Pareto.loc[_list], c=dict_colors[_trimmed_name],
	# 						alpha=alpha_Pareto) # alpha: float (0 transparent, 1 opaque)

	plt.plot(100.*x_curve, 100.*y_curve, 'k--')
	if square:  plt.axis('square')
	plt.xlabel(label_x);   	plt.ylabel(label_y)
	legend_without_duplicate_labels(ax)
	if name_range is not None: plt.title(name_range)
	if xlim is not None:  plt.xlim(xlim)
	if ylim is not None:  plt.ylim(ylim)
	plt.savefig(os.path.join('figures', nfile+(("-"+name_range) if name_range is not None else '')+".png"),
				format="png", dpi=200)
	plt.close()


def all4_vol_return_old(
		net_avg_TR, net_avg_TR_Pareto, avg_TR_curve, 
		volatility, volatility_Pareto, volatility_curve,
		avg_loss, avg_loss_Pareto, avg_loss_curve, max_loss, max_loss_Pareto, max_loss_curve,
		vol_10yr, vol_10yr_Pareto, vol_10yr_curve,
		alloc, alloc_Pareto, alloc_curve, turnover, turnover_Pareto, turnover_curve,
		# median_return_10yr, median_return_10yr_curve,
		# sd_return_10yr, sd_return_10yr_curve,
		# quartile_low_return_10yr, quartile_low_return_10yr_curve,
		score, score_Pareto, score_curve,
		allocation_current, allocation_current_Pareto, allocation_current_curve,
		name_range, ylim, alpha=alpha_all, alpha_Pareto=alpha_Pareto, dict_sources=None):

	# print(net_avg_TR);  print(avg_TR_curve);  print(volatility);  print(volatility_curve)
	vol_return(volatility, volatility_Pareto, volatility_curve,
			   net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
			   'volatility [% p.a.]', 'avg. net real returns [% p.a.]', 'volatility-return',
			   xlim=_range_volatility, ylim=ylim, name_range=name_range,
			   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	vol_return(max_loss, max_loss_Pareto, max_loss_curve,
			   net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
			   'max loss [%]', 'avg. net real returns [% p.a.]', 'loss_max-return',
			   xlim=_range_max_loss, ylim=ylim, name_range=name_range,
			   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	# vol_return(avg_loss, avg_loss_curve, net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
	# 		   'avg. loss [%]', 'avg. net real returns [% p.a.]', 'loss_avg-return',
	# 		   xlim=_range_avg_loss, ylim=ylim, name_range=name_range,
	# 		   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	vol_return(alloc, alloc_Pareto, alloc_curve,
			   net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
			   'avg. stock allocation [%]', 'avg. net real returns [% p.a.]', 'stock_alloc-return',
			   xlim=_range_allocation, ylim=ylim, name_range=name_range,
			   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	# vol_return(turnover, turnover_curve, net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
	# 		   'avg. turnover [% p.a.]', 'avg. net real returns [% p.a.]', 'turnover-return',
	# 		   xlim=(-.3, 40.), ylim=ylim, name_range=name_range,
	# 		   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	vol_return(alloc, alloc_Pareto, alloc_curve, score, score_Pareto, score_curve,
			   'avg. stock allocation [%]', 'score [%]', 'stock_alloc-score',
			   xlim=_range_allocation, ylim=_range_score, name_range=name_range,
			   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)


	vol_return(vol_10yr, vol_10yr_Pareto, vol_10yr_curve, net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
			   'volatility 10 years [% p.a.]', 'avg. net real returns [% p.a.]', 'vol_10yr-return',
			   xlim=_range_vol_10yr, ylim=ylim, name_range=name_range,
			   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	# vol_return(median_return_10yr, median_return_10yr_curve, net_avg_TR, net_avg_TR_Pareto, avg_TR_curve,
	# 		   'median return 10 years [% p.a.]', 'avg. net real returns [% p.a.]', 'median_return-return',
	# 		   xlim=ylim, ylim=ylim, name_range=name_range, square=True,
	# 		   legend_loc='upper left', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)
	#
	# vol_return(sd_return_10yr, sd_return_10yr_curve,
	# 		   quartile_low_return_10yr, quartile_low_return_10yr_curve,
	# 		   'SD return 10 years [% p.a.]', 'low quartile return 10 years [% p.a.]',
	# 		   'SD_return-quartile_return',
	# 		   xlim=[1., 7.5], ylim=[1., 5.5], name_range=name_range,
	# 		   legend_loc='upper left', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	if 'train' not in name_range:  # no 'current' in training range
		# vol_return(net_avg_TR, net_avg_TR_Pareto, avg_TR_curve, allocation_current, allocation_current_curve,
		# 		   'avg. net real returns [% p.a.]', 'current allocation [%]', 'return-current_alloc',
		# 		   xlim=ylim, ylim=_range_allocation, name_range=name_range,
		# 		   legend_loc='upper left', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

		vol_return(score, score_Pareto, score_curve,
				   allocation_current, allocation_current_Pareto, allocation_current_curve,
				   'score [%]', 'current allocation [%]', 'score-current_alloc',
				   xlim=_range_score, ylim=_range_allocation, name_range=name_range,
				   legend_loc='upper left', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	# vol_return(net_avg_TR, net_avg_TR_Pareto, avg_TR_curve, score, score_curve,
	# 		   'avg. net real returns [% p.a.]', 'score [%]', 'return-score',
	# 		   xlim=ylim, ylim=_range_score, name_range=name_range,
	# 		   legend_loc='lower right', alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	# vol_return(alloc, alloc_Pareto, alloc_curve, avg_loss, avg_loss_curve,
	# 		   'avg. stock allocation [%]', 'avg. loss [%]', 'stock_alloc-loss_avg',
	# 		   xlim=(20., 100.5), ylim=_range_avg_loss, name_range=name_range,
	# 		   legend_loc='upper left', square=True, alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)
	#
	# vol_return(alloc, alloc_Pareto, alloc_curve, volatility, volatility_Pareto, volatility_curve,
	# 		   'avg. stock allocation [%]', 'volatility [% p.a.]', 'stock_alloc-volatility',
	# 		   xlim=(20., 100.5), ylim=_range_volatility, name_range=name_range,
	# 		   legend_loc='upper left', square=True, alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)

	# vol_return(max_loss, max_loss_curve, avg_loss, avg_loss_curve,
	# 		   'max 1-year loss [%]', 'avg. losses [% p.a.]', 'loss_max-loss_avg',
	# 		   xlim=_range_max_loss, ylim=_range_avg_loss, name_range=name_range,
	# 		   legend_loc='upper left', square=True, alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)
	#
	# vol_return(volatility, volatility_Pareto, volatility_curve, avg_loss, avg_loss_curve,
	# 		   'volatility [% p.a.]', 'avg. losses [% p.a.]', 'volatility-loss_avg',
	# 		   xlim=_range_volatility, ylim=_range_avg_loss, name_range=name_range,
	# 		   legend_loc='upper left', square=True, alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)
	# vol_return(volatility, volatility_Pareto, volatility_curve,
	# 		   vol_10yr, vol_10yr_Pareto, vol_10yr_curve,
	# 		   'volatility [% p.a.]', 'volatility 10 years [% p.a.]', 'volatility-vol_10yr',
	# 		   xlim=_range_volatility, ylim=_range_vol_10yr, name_range=name_range,
	# 		   legend_loc='lower right', square=True, alpha=alpha, alpha_Pareto=alpha_Pareto, dict_sources=dict_sources)



def all4_vol_return(statistics, statistics_Pareto, statistics_curve,
					name_range, ylim, alpha=alpha_all, alpha_Pareto=alpha_Pareto, dict_sources=None):
	# print(name_range, 'statistics [%]');  print((100*statistics).round(2))
	return all4_vol_return_old(
		statistics['net_avg_TR_actual'],statistics_Pareto['net_avg_TR_actual'],statistics_curve['net_avg_TR_actual'],
		statistics['volatility'],statistics_Pareto['volatility'],statistics_curve['volatility'],
		statistics['avg_loss'],	 statistics_Pareto['avg_loss'],	 statistics_curve['avg_loss'],
		statistics['max_loss'],	 statistics_Pareto['max_loss'],	 statistics_curve['max_loss'],
		statistics['vol_10yr'],	 statistics_Pareto['vol_10yr'],	 statistics_curve['vol_10yr'],
		statistics['allocation'],statistics_Pareto['allocation'],statistics_curve['allocation'],
		statistics['turnover'],	 statistics_Pareto['turnover'],	 statistics_curve['turnover'],
		# statistics['median_return_10yr'],	statistics_curve['median_return_10yr'],
		# statistics['sd_return_10yr'],		statistics_curve['sd_return_10yr'],
		# statistics['quartile_low_return_10yr'],statistics_curve['quartile_low_return_10yr'],
		statistics['score_actual'],	 statistics_Pareto['score_actual'],	 statistics_curve['score_actual'],
		statistics['allocation_current'],statistics_Pareto['allocation_current'],
			statistics_curve['allocation_current'],
		name_range, ylim, alpha, alpha_Pareto, dict_sources)


# print(dict_statistics_df['all'].index[idx_efficient_train])


## Plot time evolution

def time_evolution(dict_strategies):

	# plot allocation(time)
	_alloc = pd.DataFrame.from_dict({_name: dict_strategies[_name].allocation_df.loc[range_train]
	 					 for _name in dict_strategies})
	time_allocations	  (_alloc, 'best of each source (training)', 'best_each_source-train')
	time_stdev_allocations(_alloc, 'best of each source (training)', 'best_each_source-train')

	_alloc = pd.DataFrame.from_dict({_name: dict_strategies[_name].allocation_df.loc[range_valid]
	 					 for _name in dict_strategies})
	time_allocations	  (_alloc, 'best of each source (validation)', 'best_each_source-valid')
	time_stdev_allocations(_alloc, 'best of each source (validation)', 'best_each_source-valid')

	# _alloc = pd.DataFrame.from_dict({_name: dict_strategies[_name].allocation_df.loc[range_train]
	# 					 for _name in set_names_max_score}).rolling(window=months_SMA, center=True, axis=0).mean()
	# (100*_alloc).plot()
	# plt.title('best of each source (training)')
	# plt.ylabel('allocation [%]');	plt.ylim(-0.5, 100.5)
	# plt.savefig(os.path.join('figures', 'time-allocation-train.png'), format='png', dpi=200)
	# plt.close()
	#
	# _alloc = pd.DataFrame.from_dict({_name: dict_strategies[_name].allocation_df.loc[range_valid]
	# 					 for _name in set_names_max_score}).rolling(window=months_SMA, center=True, axis=0).mean()
	# (100*_alloc).plot()
	# plt.title('best of each source (validation)')
	# plt.ylabel('allocation [%]');	plt.ylim(-0.5, 100.5)
	# plt.savefig(os.path.join('figures', 'time-allocation-valid.png'), format='png', dpi=200)
	# plt.close()


	# plot TR(time)
	# set_names_max_score.add('alloc100_0')
	# _TR_df.plot()
	# plt.semilogy();  # plt.ylim(0.8, 200.)
	# plt.title('best of each source (training)');  plt.legend(loc='upper left')
	# plt.savefig(os.path.join('figures', 'time-capital-train.png'), format='png', dpi=200)
	# plt.close()


	# _TR_df.plot()
	# plt.semilogy();  plt.ylim(0.8, 100.)
	# plt.title('best of each source (validation)');  plt.legend(loc='upper left')
	# plt.savefig(os.path.join('figures', 'time-capital-valid.png'), format='png', dpi=200)
	# plt.close()


