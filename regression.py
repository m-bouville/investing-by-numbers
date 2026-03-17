import os, sys
# import warnings
import copy

# import cvxopt
from statsmodels.graphics.regressionplots import plot_regress_exog
from statsmodels.formula.api import ols

from sklearn.model_selection import train_test_split
from sklearn.ensemble 		 import RandomForestRegressor
from sklearn.metrics 		 import r2_score

from scipy import spatial

import pandas  as pd
import numpy   as np
from   seaborn import heatmap as sns_heatmap
# import cvxopt

import matplotlib.pyplot as plt


import utils, plots

# TODO Plot average
# TODO account for drawdown: if market fell by 30-40% I should be ready to jump back in [FAILS]


os.chdir('D:/finances/investing by numbers')

csv_sep = ";"
all_data = utils.import_data('SP500.csv', sep=csv_sep, decimal=".")

quick_test = False

_increment = 10 if not quick_test else None
list_allocation_targets	   = np.linspace (0.3,   1.1, _increment) if not quick_test else [.4,  .6,  .8]
list_allocation_amplitudes = np.geomspace(10.,  55,   _increment) if not quick_test else [10., 20., 40]
_nb_cases = len(list_allocation_targets) * len(list_allocation_amplitudes)

list_lengths 		= [1, 2, 'avg_1_7yrs', 5, 10, 'avg_9_15yrs', 15, 20]  \
							if not quick_test else [10, 15, 'avg_9_15yrs']  # 'avg_1_7yrs',
list_DD				= ['TR_min']  #'inv_TR_ratio_1yr', 'inv_TR_ratio', 'time_since_max']

train_size 			= 15.  / 100
max_date_training	= 1965
utils.trading_cost_policy  =  .5 / 100.  # TODO change to 0.5%, 1%, 2% (3%?)
utils.trading_cost_actual  =  2.  / 100.  # do NOT change between simulations
net_return_threshold=  6.5 / 100  if not quick_test else 6.  /100

score_min_Pareto	=  0.4 / 100  if not quick_test else 0.2 /100
score_incr_Pareto	=  0.2 / 100
min_nb_input_quantiles= 10

show_predictions    = True
regressions_to_show = []  # list_lengths  # [5, 10, 15]
_missing = [e for e in regressions_to_show if e not in list_lengths]
assert len(_missing) == 0, _missing

list_stock_allocation = (list(np.arange(0.1, 0.51, 0.05)) + [0.6, 0.7, 0.8, 1.]) \
	if not quick_test else [0.25, 0.4, 0.7, 1.]

names_strategies_selected = ['decl_hi-avg_of_avgs',    'max_dec_hi-avg_of_avgs',
							 'max_dec_med-avg_of_avgs',   'qrtl_hi-avg_of_avgs',
							 'max_hi_med-avg_of_avgs', 'max_hi_low-avg_of_avgs',
							 'median-avg_of_avgs',  ]
# 'avg-avg_of_avgs', 'qrtl_mid-avg_of_avgs', 'max_mid-avg_of_avgs', 'qrtl_low-avg_of_avgs']

return_min_pc		= 3.5  # corresponding to minimum Markowitz allocation
return_max_pc		= 8.

# Random forest parameters
min_samples_split	=  8
min_samples_leaf	=  5
max_samples			=   .5
max_features		= 'sqrt'

min_R2_train 		= 0.45

plot_individual_parametrizations = False

# remove variables not meant to be used in regression
real_TR  = all_data[['real_TR']];   real_TR.columns = ['SP500']
DD 		 = all_data[['TR_min', 'ln_TR_min', 'inv_TR_ratio', '-ln_TR_ratio', 'inv_TR_ratio_1yr',
					   'time_since_max']]
real_interest_rate_10yr = all_data[['real_interest_rate_10yr']]
all_data.drop(columns=['real_TR'], inplace=True)

# remove variables that cannot be used for current prediction
# all_data.drop(columns=['TR_ratio'], inplace=True)

# # remove variables that are a source of multi-collinearity
all_data.drop(columns=['real_return_avg_1_7yrs', 'real_return_avg_9_15yrs'], inplace=True)

names_future_returns = ['future_real_return_' + str(length) + 'yr' for length in [1, 2, 5, 10, 15, 20]] + \
					   ['future_real_return_avg_1_7yrs', 'future_real_return_avg_9_15yrs',
						'TR_min', 'ln_TR_min', 'inv_TR_ratio', '-ln_TR_ratio'] + \
					   ['inv_TR_ratio_1yr', 'inv_TR_ratio_2yr', 'inv_TR_ratio_5yr',
						'time_since_max']
# list(range(9, 13+1))


_list_variables = [ 'ln_TR_max', 'TR_max_off5yr', 'TR_max_off_m5yr',
					'real_return_7yr', 'real_return_15yr',  'real_interest_rate_10yr',
 					'BB_avg',  'sqrt_excess_CAPE_yield', 'sd_avg', 'SMA_ratio_avg', 'curv_avg',
					'detrended_BB_avg',  'detrended_SMA_avg', 'detrended_off5yr', 'detrended_off10yr',
					'detrended',  'detrended_sqr',
					'curv_2yr',
					'detrended_plus_slope_2yrs', 'detrended_plus_slope_5yrs',
					'return_avg_normalized', 'stdev_avg_normalized',
					# troughs:
					'real_return_10yr_cutoff_2pc', 'SMA_real_return_10yr_cutoff_m10pc',
					'real_return_6mth_cutoff_m12pc','real_return_1yr_cutoff_m20pc',
					'detrended_cutoff_low_03',
					# peaks:
					'real_return_10yr_cutoff_11pc', 'SMA_real_return_15yr_cutoff_150pc',
					'detrended_cutoff_hi_03',
				   ]
all_data = all_data[_list_variables + names_future_returns]

# _list_variables_short = [ 'BB_avg',  'curv_avg',  'ln_TR_max',
# 						  'detrended',  'detrended_BB_avg',  'detrended_SMA_avg',
# 						  'detrended_off5yr', 'detrended_plus_slope_2yrs'
# 						  ]   # selected for 1 and 2 years
#
# _list_variables_long = [ 'TR_max_off_m5yr',  'curv_2yr',  'curv_avg',  'ln_TR_max',  'real_return_7yr',
# 						 'detrended',  'detrended_off5yr', 'detrended_plus_slope_2yrs',
# 						 'detrended_cutoff_low_03',  'detrended_cutoff_hi_03',
# 						  ]   # selected for 15 and 20 years
#
# _list_variables_all = [ 'curv_avg',  'ln_TR_max', 'real_return_7yr',
# 						  'detrended',
# 						  'detrended_off5yr', 'detrended_plus_slope_2yrs'
# 						  ]   # selected based on all lengths
#
# all_data = all_data[_list_variables_all + _list_variables_short + _list_variables_long + names_future_returns]
# all_data = all_data.T.drop_duplicates().T


# list_lengths = [1, 2]
# list_DD = []
#
# names_future_returns = ['future_real_return_' + str(length) + 'yr' for length in list_lengths] +\
# 					   ['inv_TR_ratio_1yr', 'inv_TR_ratio_2yr']
#
# _list_variables = [ 'real_return_1yr', # 'real_return_2yr', # 'real_return_5yr',
# 					'curv_2yr',
# 					'sd_2yr', 'sd_1yr', # 'sd_6mo',
# 					'BB_2yr', # 'BB_6mo',
# 					# 'detrended_BB_avg',
# 					'detrended_sqr',
# 					'real_return_6mth_cutoff_m12pc', 'real_return_1yr_cutoff_m20pc',
# 					'detrended_cutoff_low_03', 'detrended_cutoff_hi_03',
# 				   ]



# remove variables missing a lot of data at the beginning
# all_data.drop(columns=['real_return_15yr'], inplace=True)



names_X = all_data.columns.difference(names_future_returns)
# print(names_X)

# remove incomplete dates
all_data = all_data[~all_data[names_X].isna().any(axis=1)]
utils.all_data = all_data
real_TR 				= real_TR				 .loc[all_data.index]
real_interest_rate_10yr = real_interest_rate_10yr.loc[all_data.index]
ratio_SP   				= real_TR.div(real_TR.shift(1)).squeeze();  utils.ratio_SP = ratio_SP

# print(real_interest_rate_10yr.squeeze())
ratio_obli = real_interest_rate_10yr / 12.;   utils.ratio_obli = ratio_obli  # TODO check
# real_interest_rate_10yr.plot();  plt.show()


# correlation matrix
# print(len(names_X), 'variables')
if len(names_X) < 12:
	cor = 100. * all_data[names_X].corr()
	# sns_heatmap(cor, square=True, cmap="coolwarm", linewidths=.5, annot=True, vmin=-100., vmax=100.)
	# plt.title('correlation variables [%]')
	# plt.savefig(os.path.join('figures', 'correlation-variables.png'), format="png", dpi=200)
	# plt.close()

	sns_heatmap(cor.abs(), square=True, cmap="coolwarm", linewidths=.5, annot=True, vmin=0., vmax=100.)
	plt.title('|correlation variables| [%]')
	plt.savefig(os.path.join('figures', 'correlation-abs_variables.png'), format="png", dpi=200)
	plt.close()


# Split the data into training and testing sets
train_data, _ = train_test_split(all_data[all_data.index < max_date_training],
								 train_size = train_size, random_state = 42)
# print('train_data.index:', train_data.index)


# remove incomplete dates at the end
start_date_recent = 1994
range_train = list(all_data.index <  max_date_training)
range_valid = list(all_data.index >= max_date_training)
range_recent= list(all_data.index >= start_date_recent)
dict_ranges = {'all': list(all_data.index), 'train': range_train,
			   'valid': range_valid, 'recent': range_recent}
utils.range_train = range_train;   plots.range_train = range_train
utils.range_valid = range_valid;   plots.range_valid = range_valid
utils.range_recent= range_recent;  plots.range_recent= range_recent
utils.dict_ranges = dict_ranges;   plots.dict_ranges = dict_ranges

clean_train_data = train_data.loc[~train_data.isna().any(axis=1)]
valid_data 		 = all_data  .loc[range_valid]
range_clean_valid= valid_data.index[~valid_data.isna().any(axis=1)]
clean_valid_data = valid_data .loc[~valid_data.isna().any(axis=1)]
utils.clean_valid_data = clean_valid_data;  utils.range_clean_valid = range_clean_valid

start_year 		 = min(all_data.loc[range_train].index) - .5
plots.start_year = start_year


# from sklearn import preprocessing
# import matplotlib.pyplot as plt
#
# _df = all_data[_list_variables].rolling(6).mean()
# x_scaled = pd.DataFrame(preprocessing.MinMaxScaler().fit_transform(_df) - 0.5,
# 						columns=_list_variables, index=all_data.index).loc[range_valid]
# x_scaled.plot()
#
# x_scaled = pd.DataFrame(preprocessing.StandardScaler().fit_transform(_df),
# 						columns=_list_variables, index=all_data.index).loc[range_valid]
# x_scaled.plot(ylim=[-2, 2])
# plt.show()



# predictions
#############

dict_predictions = dict()

for length in list_lengths + list_DD:
	# if length in [1, 2, 3]:
	# 	names_X = _list_variables_short
	# elif length in [15, 20, 25, 30]:
	# 	names_X = _list_variables_long
	# else:
	# 	names_X = _list_variables_all


	_name_y = utils.name_y_df(length)

	## linear regression
	my_formula= _name_y + '~' + "+".join(names_X)
	# print('\n'+my_formula)

	model_linear = ols(formula=my_formula, data=clean_train_data).fit_regularized(method="sqrt_lasso")

	_prediction_linear = model_linear.predict(all_data)
	_R2_linear_train   = r2_score(clean_train_data[_name_y], model_linear.predict(clean_train_data))
	_R2_linear_valid   = r2_score(clean_valid_data[_name_y], model_linear.predict(clean_valid_data))

	if length in regressions_to_show:
		model_linear = ols(formula=my_formula, data=clean_train_data).fit()
		print(model_linear.summary(alpha=0.1, slim=True))
	elif isinstance(length, int):
		# print(model_linear.predict(clean_valid_data.iloc[[-6, -5, -4, -3, -2, -1]]))
		_returns_pc = [round(100*e, 3) for e in model_linear.predict(clean_valid_data.iloc[[-5, -4, -3, -2, -1]]).values]
		# print(_returns_pc)
		print(f"{length:2n} years:{sum(_returns_pc)/len(_returns_pc):5.1f}% "
			f"({min(_returns_pc):5.1f}% to{max(_returns_pc):5.1f}%) p.a. real")
			# f"(R^2 ={_R2_linear_valid * 100:5.1f}%)")


	# Random forest
	model_rf = RandomForestRegressor(n_estimators=200,
		min_samples_split=min_samples_split, max_samples =max_samples,
		min_samples_leaf =min_samples_leaf,  max_features=max_features, n_jobs=-1)  # , random_state=42)
	model_rf.fit(clean_train_data[names_X], clean_train_data[_name_y])
	_prediction_rf = pd.Series(model_rf.predict(all_data[names_X]), index=all_data.index, dtype='float64')
	# print('predict_rf:\n', predictions_rf[length])
	# predictions_rf[length].plot(); plt.show()
	_R2_rf_train = r2_score(clean_train_data[_name_y], model_rf.predict(clean_train_data[names_X]))
	_R2_rf_valid = r2_score(clean_valid_data[_name_y], model_rf.predict(clean_valid_data[names_X]))

	_y 			= utils.name_y(length)
	_name_linear= _y + '-linear'
	_name_rf 	= _y + '-rf'
	_type_y		= None
	_l			= None

	if length in list_lengths:
		_type_y = 'return';  _l = length
		dict_predictions[_name_linear] = utils.Prediction(
				_name_linear, _type_y, _l, 'linear', False, _prediction_linear, _R2_linear_train, _R2_linear_valid)

		dict_predictions[_name_rf] 	   = utils.Prediction(
				_name_rf, 	  _type_y, _l, 'rf',     False, _prediction_rf, 	_R2_rf_train,     _R2_rf_valid)


	# allocation_linear[length] = allocation_from_return(
		# 	_prediction, allocation_target=allocation_target, allocation_amplitude=allocation_amplitude)

	elif 'TR_' in length:   # must be <= 1 and >= 1/2 (a drawdown worse than 50% can be treated as 50%)
		if length[-2:] == 'yr':
			_type_y = length[:-4];  _l = length[-3]
		else:
			_type_y = length;  _l = None

		if 'ln_' in length:
			_prediction_linear = _prediction_linear.clip(lower=-np.log(2.5), upper=0.)
			dict_predictions[_name_linear] = utils.Prediction(
					_name_linear, _type_y, _l, 'linear', True,  _prediction_linear, _R2_linear_train, _R2_linear_valid)

			_prediction_rf = _prediction_rf.clip(lower=-np.log(2.5), upper=0.)
			dict_predictions[_name_rf] = utils.Prediction(
					_name_rf, _type_y, _l, 'rf', True, _prediction_rf, _R2_rf_train,     _R2_rf_valid)
		else:
			_prediction_linear = _prediction_linear.clip(lower=1/2.5, 	   upper=1.)
			dict_predictions[_name_linear] = utils.Prediction(
					_name_linear, _type_y, _l, 'linear', False, _prediction_linear, _R2_linear_train, _R2_linear_valid)

			_prediction_rf = _prediction_rf.clip(lower=1/2.5, 	   upper=1.)
			dict_predictions[_name_rf] = utils.Prediction(
					_name_rf, _type_y, _l, 'rf', False, _prediction_rf, _R2_rf_train,     _R2_rf_valid)

	elif length == 'time_since_max':  # must be >= 0
		_type_y = 'time_since_max';   _l = None
		dict_predictions[_name_linear] = utils.Prediction(_name_linear, 'time_since_max',
					None, 'linear', False, _prediction_linear.clip(lower=0.), _R2_linear_train, _R2_linear_valid)

		dict_predictions[_name_rf] 	   = utils.Prediction(_name_rf,     'time_since_max',
					None, 'rf',     False, _prediction_rf    .clip(lower=0.), _R2_rf_train,     _R2_rf_valid)


	# linear + rf
	if _name_linear in dict_predictions and _name_rf in dict_predictions:
		_predictions = pd.concat([ dict_predictions[_name_linear].prediction_df,
								   dict_predictions[_name_rf]    .prediction_df], axis=1)
		_predictions.columns = [_name_linear, _name_rf]
		_prediction_meta2, _R2_meta2_train, _R2_meta2_valid, _ = utils.prediction_meta_one_y(
			_predictions, _name_y, {_name_linear: _R2_linear_train, _name_rf: _R2_rf_train})

	# remove inefficient strategies
	if _R2_linear_train < min_R2_train:  dict_predictions.pop(_name_linear)
	if _R2_rf_train     < min_R2_train:  dict_predictions.pop(_name_rf)
	if _R2_meta2_train >= min_R2_train:
		dict_predictions[_y+'-lin+rf'] = utils.Prediction(
			_y+'-lin+rf', _type_y, _l, 'linear+rf', False, _prediction_meta2, _R2_meta2_train, _R2_meta2_valid)

	# print('{:24s}-- R² training: linear {:.1f}%, RF {:.1f}%, lin+RF {:.1f}%'.
	# 	  format(_name_y, 100*_R2_linear_train, 100*_R2_rf_train, 100*_R2_meta2))


	# plot partial residuals
	if (length in ['TR_ratio', 'time_since_max'] or 'avg' in str(length)) and \
			len(names_X) < 10 and plot_individual_parametrizations:
		for _name_var in names_X:
			fig = plt.figure()
			fig = plot_regress_exog(model_linear, _name_var, fig=fig)
			plt.savefig(os.path.join('figures', 'residuals', utils.name_png(length)+'-'+_name_var+".png"),
						format="png", dpi=200)
			plt.close()

utils.dict_predictions = dict_predictions

if show_predictions:
	utils.Prediction.print_headings()
	for _name in dict_predictions:
		dict_predictions[_name].print_one_line()



## plots predictions
# scatter: prediction as function of actual
# train
print("Plotting prediction as function of actual, for training")
# TODO Ensure they really have the same reference
_name_y = 'return_avg_9_15yrs'
if _name_y+'-linear' in dict_predictions:
	_predict = pd.concat([dict_predictions[_name_y+'-linear'].prediction_df,
						  dict_predictions[_name_y+'-rf'	].prediction_df,
						  dict_predictions[_name_y+'-lin+rf'].prediction_df], axis=1)
	_predict.columns = ['linear regression', 'random forest', 'lin + RF']
	# print(result_train['actual']);  print(dict_predict)
	plots.actual_predicted(all_data.loc[range_train]['future_real_'+_name_y],
						   _predict.loc[range_train], _name_y, 'training')
	plots.actual_predicted(all_data.loc[range_valid]['future_real_'+_name_y],
						   _predict.loc[range_valid], _name_y, 'valid')


	# scatter: residual as function of actual
	my_min = min(_predict.min())
	my_max = max(_predict.max())
	# print(my_min, my_max)

	_residual = pd.concat([ _predict['linear regression']- all_data['future_real_'+_name_y],
							_predict['random forest']    - all_data['future_real_'+_name_y],
							_predict['lin + RF']         - all_data['future_real_'+_name_y]], axis=1)
	_residual.columns = ['linear regression', 'random forest', 'lin + RF']

	plots.actual_residual(all_data.loc[range_train]['future_real_'+_name_y], _residual.loc[range_train],
						  _name_y, 'training', [my_min, my_max])
	plots.actual_residual(all_data.loc[range_valid]['future_real_'+_name_y], _residual.loc[range_valid],
						  _name_y, 'valid',    [my_min, my_max])


# # validation
# print("Plotting prediction as function of actual, for validation")
# result_valid = result.loc[valid_data.index]
# dict_predict_valid = {'linear regression': result_valid['predict_linear'],
# 					  'random forest':     result_valid['predict_rf'],
# 					  'meta2':             result_valid['predict_meta2']}
# # print(result_valid['actual']);  print(dict_predict)
# plots.actual_predicted(result_valid['actual'], dict_predict_valid, _name_y, 'validation')
#
#
# # scatter: residual as function of actual
# my_min = min([min(dict_predict_valid[_name]) for _name in dict_predict_valid])
# my_max = max([max(dict_predict_valid[_name]) for _name in dict_predict_valid])
#
# plots.actual_residual(result_valid['actual'], \
# 						{'linear regression': result_valid['residual_linear'],
# 						 'random forest':     result_valid['residual_rf'],
# 						 'meta2':             result_valid['residual_meta2']},
# 					  _name_y, 'validation', [my_min, my_max])





# strategies
###########

dict_strategies = dict()


print(len(list_allocation_targets),    'targets *',
	  len(list_allocation_amplitudes), 'amplitudes =', _nb_cases, 'cases')

# utils.Strategy.print_headings(range='train')

# constant allocations stocks-bonds
for _stock_allocation in list_stock_allocation:
	_name_strategy = 'alloc' + str(int(round(100.*   _stock_allocation))) \
					 + '_'   + str(int(round(100.*(1-_stock_allocation))))
	_TR = utils.TR_from_constant_allocation(_stock_allocation, _name_strategy, all_data.index).T
	dict_strategies[_name_strategy] = \
		utils.Strategy(_name_strategy, 'constant', 0, _stock_allocation, TR_df=_TR, range='train')


# # calculating slopes for score
# utils.slopes_for_score(dict_strategies)
# print({_range: round(np.mean([dict_strategies[_name].score[_range]
# 	for _name in ['alloc60_40', 'alloc70_30', 'alloc80_20']]), 6) for _range in ['all', 'train', 'valid']})
# sys.exit()


_names_predictions_lin_rf = list(dict_predictions.keys())
		#[_name for _name in dict_predictions if 'lin+rf' in _name]
_dict_targets 	 = pd.Series([0] * len(list_allocation_targets),    index=list_allocation_targets)
_dict_amplitudes = pd.Series([0] * len(list_allocation_amplitudes), index=list_allocation_amplitudes)
_dict_targets_high_returns    = _dict_targets   .copy()
_dict_amplitudes_high_returns = _dict_amplitudes.copy()

for _target in list_allocation_targets:
	print('allocation target: {:5.1f}%'.format(round(100.*_target, 1)))

	# if not quick_test: utils.Strategy.print_headings(range='train')
	for _amplitude_ref in list_allocation_amplitudes:

		## allocations based on future returns
		_name_para = ('m' if _target 	   < 0 else '') + str(round(abs(_target)*100)) + 'pc_' + \
					 ('m' if _amplitude_ref< 0 else '') + str(round(abs(_amplitude_ref)))

		# print("Calculating allocations")

		for _source in _names_predictions_lin_rf:
			_name_strategy = _source + '-' + _name_para
			dict_strategies[_name_strategy] = \
				utils.Strategy(_name_strategy, _source, _amplitude_ref, _target)
			if dict_strategies[_name_strategy].score_policy['train'] >= score_min_Pareto:
				_dict_targets   [_target] 		 += 1
				_dict_amplitudes[_amplitude_ref] += 1
				if dict_strategies[_name_strategy].net_avg_TR_policy['train'] >= net_return_threshold:
					_dict_targets_high_returns   [_target] 		  += 1
					_dict_amplitudes_high_returns[_amplitude_ref] += 1

		my_mat = pd.concat([dict_strategies[_source + '-' + _name_para].allocation_df \
							for _source in _names_predictions_lin_rf], axis=1)
		dict_strategies['meta3-'+_name_para] = utils.Strategy(
			'meta3-'+_name_para, 'quantile', _amplitude_ref, _target, allocation_df=my_mat.mean(axis=1))
		dict_strategies['min-'  +_name_para] = utils.Strategy(
			'min-' + _name_para, 'quantile', _amplitude_ref, _target, allocation_df=my_mat.min (axis=1))
		dict_strategies['max-'  +_name_para] = utils.Strategy(
			'max-' + _name_para, 'quantile', _amplitude_ref, _target, allocation_df=my_mat.max (axis=1))

_title = 'fraction [%] with training score >= ' + str(round(100*score_min_Pareto, 2)) + '%'
_norm = 1 / len(list_allocation_amplitudes) / len(_names_predictions_lin_rf) * 100.
_dict_targets *= _norm;  _dict_targets_high_returns *= _norm
_dict_targets.index = [100.*e for e in _dict_targets.index]
_dict_targets_high_returns.index = _dict_targets.index
_dict_targets.plot();  _dict_targets_high_returns.plot()
plt.ylim(0, max(_dict_targets)*1.05);  plt.xlabel('targets [%]');  plt.ylabel(_title)
plt.savefig(os.path.join('figures', "good_targets.png"), format="png", dpi=200);	plt.close()

_norm = 1 / len(list_allocation_targets) / len(_names_predictions_lin_rf) * 100.
_dict_amplitudes *= _norm;  _dict_amplitudes_high_returns *= _norm
_dict_amplitudes.plot();  _dict_amplitudes_high_returns.plot()
plt.ylim(0, max(_dict_amplitudes)*1.05);  plt.semilogx()
plt.xlabel('amplitudes');  plt.ylabel(_title)
plt.savefig(os.path.join('figures', "good_amplitudes.png"), format="png", dpi=200);  plt.close()


print('\nTiming:')
print('Allocation     took{:6.1f} s.'.format(utils.Strategy.duration_alloc))
print('   incl. TR_xx DD  {:6.1f} s.'.format(utils.Strategy.duration_alloc_DD))
print('Time evolution took{:6.1f} s.'.format(utils.Strategy.duration_TR))


dict_sources = {_name: dict_strategies[_name].source for _name in dict_strategies}


# Statistics
############

# Prepare empty containers for column frames instead of building DataFrames column-by-column
stats_cols_all   = []
stats_cols_train = []
stats_cols_valid = []

curve_cols_all   = []
curve_cols_train = []
curve_cols_valid = []

for _name, _strategy in dict_strategies.items():
    if _strategy.source == 'constant':
        # allocation is constant — same allocation key for all ranges
        _alloc = _strategy.allocation_avg['all']

        col_all, col_train, col_valid = _strategy.split_strategy()

        # each returned result becomes its own dataframe-column
        curve_cols_all  .append(pd.DataFrame({ _alloc: col_all  }))
        curve_cols_train.append(pd.DataFrame({ _alloc: col_train}))
        curve_cols_valid.append(pd.DataFrame({ _alloc: col_valid}))

    else:
        col_all, col_train, col_valid = _strategy.split_strategy()

        stats_cols_all  .append(pd.DataFrame({ _name: col_all  }))
        stats_cols_train.append(pd.DataFrame({ _name: col_train}))
        stats_cols_valid.append(pd.DataFrame({ _name: col_valid}))

# Build the final DataFrames using concat once → NO fragmentation
dict_statistics_df = {
    'all':   pd.concat(stats_cols_all,   axis=1) if stats_cols_all   else pd.DataFrame(),
    'train': pd.concat(stats_cols_train, axis=1) if stats_cols_train else pd.DataFrame(),
    'valid': pd.concat(stats_cols_valid, axis=1) if stats_cols_valid else pd.DataFrame(),
}

dict_statistics_curve_df = {
    'all':   pd.concat(curve_cols_all,   axis=1) if curve_cols_all   else pd.DataFrame(),
    'train': pd.concat(curve_cols_train, axis=1) if curve_cols_train else pd.DataFrame(),
    'valid': pd.concat(curve_cols_valid, axis=1) if curve_cols_valid else pd.DataFrame(),
}


# dict_statistics_df 		 = {'all': pd.DataFrame(), 'train': pd.DataFrame(), 'valid': pd.DataFrame()}
# dict_statistics_curve_df = {'all': pd.DataFrame(), 'train': pd.DataFrame(), 'valid': pd.DataFrame()}

# for _name in dict_strategies:
# 	_strategy = dict_strategies[_name]
# 	_list = dict()
# 	if _strategy.source == 'constant':
# 		_alloc = _strategy.allocation_avg['all']  # allocation is constant, so same for all ranges
# 		dict_statistics_curve_df['all']  [_alloc], dict_statistics_curve_df['train'][_alloc],\
# 		dict_statistics_curve_df['valid'][_alloc] = _strategy.split_strategy()
# 	else:
# 		dict_statistics_df['all'  ][_name], dict_statistics_df['train'][_name], \
# 		dict_statistics_df['valid'][_name] = _strategy.split_strategy()

_columns = ['net_avg_TR_policy', 'net_avg_TR_actual', 'volatility', 'avg_loss', 'max_loss', 'max_loss_year', 'vol_10yr',
			'allocation', 'turnover', 'score_policy', 'score_actual', 'allocation_current']
for _range in ['all', 'train', 'valid']:
	dict_statistics_df		[_range] = dict_statistics_df	   [_range].T
	dict_statistics_curve_df[_range] = dict_statistics_curve_df[_range].T
	dict_statistics_df		[_range].columns = _columns
	dict_statistics_curve_df[_range].columns = _columns

# print(dict_statistics_df['train'])
# print(dict_statistics_curve_df['train'])

list_sources = [];  list_trimmed_sources = []
for _name in dict_strategies:
	_name_source = dict_sources[_name]
	if _name_source not in list_sources and _name_source != 'constant':
		list_sources.append(_name_source)
		if utils.trimmed_name(_name_source) not in list_trimmed_sources:
			list_trimmed_sources.append(utils.trimmed_name(_name_source))

list_sources.append('quantile')
# print('list_sources:', list_sources)
plots.list_sources 		   = list_sources
plots.list_trimmed_sources = list_trimmed_sources

print(len(dict_statistics_df['all']), 'strategies')
_statistics_all = copy.deepcopy(dict_statistics_df)
# for _range in ['train', 'valid']:  # 'all',
# 	_name = 'training' if _range == 'train' else ('validation' if _range == 'valid' else _range)
# 	plots.all4_vol_return(
# 		dict_statistics_df[_range], {e: None for e in dict_statistics_df[_range]},
# 		dict_statistics_curve_df[_range],
# 		name_range=_name, ylim=(return_min_pc, return_max_pc), alpha=plots.alpha_all,
# 		dict_sources=dict_sources)


# sole selection criterion: scores; so threshold has to be more stringent
_score_min = score_min_Pareto + 1.5 * score_incr_Pareto
_name = 'score_min_'+str(round(100*_score_min,3))+'%'
_dict_strategies, _statistics_df, _nb_input = \
	utils.average_Pareto(dict_strategies, _score_min, _columns, _name)
print(_nb_input, 'strategies in', _name)
if _nb_input >= min_nb_input_quantiles:  # statistics based on enough input
	for _range in ['all', 'train', 'valid']:
		if not _statistics_df[_range].empty:
			dict_statistics_df[_range]	 = pd.concat([dict_statistics_df[_range],
														_statistics_df[_range]], axis=0)
if _dict_strategies:  dict_strategies.update(_dict_strategies)




# Pareto front
##############

costs_train = dict_statistics_df['train'][['volatility', 'vol_10yr']]  # , 'max_loss']]
# low is good:
costs_train['neg-net_avg_TR_policy'] = -dict_statistics_df['train']['net_avg_TR_policy']

# print('costs_train [%]:\n', (100.*costs_train).round(2))

idx_efficient_train = list(utils.is_pareto_efficient(costs_train.to_numpy(), return_mask=False))
print(len(idx_efficient_train), 'strategies (Pareto front),',
	  	round(len(idx_efficient_train)/len(_statistics_all['all'])*100, 1), '%')
# print('idx_efficient_train:', idx_efficient_train)

_names_efficient_train = [dict_statistics_df['train'].index[i] for i in idx_efficient_train]
_dict_strategies_train_Pareto = {e: dict_strategies[e] for e in _names_efficient_train}

# print('starting `average_Pareto` for `_dict_strategies_train_Pareto`...')
# two selection criteria: Pareto front + scores; so threshold does not have to be too stringent
_score_min = score_min_Pareto + score_incr_Pareto
_name = 'Pareto-score_min_'+str(round(100*_score_min,3))+'%'
_dict_strategies, _statistics_df, _nb_input = \
	utils.average_Pareto(_dict_strategies_train_Pareto, _score_min, _columns, _name)
# print('`average_Pareto` for `_dict_strategies_train_Pareto` done.')
print(_nb_input, 'strategies in', _name)
if _nb_input >= min_nb_input_quantiles:  # statistics based on enough input
	for _range in ['all', 'train', 'valid']:
		if not _statistics_df[_range].empty:
			dict_statistics_df[_range] = pd.concat(
				[dict_statistics_df[_range], _statistics_df[_range]], axis=0)
if _dict_strategies:  dict_strategies.update(_dict_strategies)


dict_sources = {_name: dict_strategies[_name].source for _name in dict_strategies}


# print('dict_statistics_df['train'] [%]:\n', (100.*dict_statistics_df['train']).round(2))
_len=len(dict_statistics_df['train'].index)

## convex hull
idx_convex_train = [idx_efficient_train[i] for i in list(spatial.ConvexHull(
	costs_train.iloc[idx_efficient_train].to_numpy()).vertices)] + \
				   [_len-4+e for e in range(0, 4)]
		# [:-3]: remove avg, min and max based on the whole front
print(len(idx_convex_train), 'strategies (Pareto front, convex hull)')
# print('idx_convex_train:', idx_convex_train)

## avg, min and max Pareto

_names_convex_train = [dict_statistics_df['train'].index[i] for i in idx_convex_train]
_dict_strategies_convex_train = {e: dict_strategies[e] for e in _names_convex_train}

# with selection criteria Pareto front + convex hull, almost all have a high score anyway
_score_min = score_min_Pareto
_name = 'Pareto_convex-score_min_'+str(round(100*_score_min,3))+'%'
_dict_strategies, _statistics_df, _nb_input = \
	utils.average_Pareto(_dict_strategies_convex_train, _score_min, _columns, _name)
print(_nb_input, 'strategies in', _name)
if _nb_input >= min_nb_input_quantiles:  # statistics based on enough input
	for _range in ['all', 'train', 'valid']:
		if not _statistics_df[_range].empty:
			dict_statistics_df[_range] = pd.concat(
				[dict_statistics_df[_range], _statistics_df[_range]], axis=0)
if _dict_strategies:  dict_strategies.update(_dict_strategies)

dict_sources = {_name: dict_strategies[_name].source for _name in dict_strategies}

_len = len(dict_statistics_df['train'].index)


idx_good_train = [i for i in idx_convex_train
		  if dict_strategies[dict_statistics_df['train'].index[i]].score_policy['train'] >= score_min_Pareto] \
				 + [_len-4+e for e in range(0, 4)]
print(len(idx_good_train), 'strategies in convex hull w/ training score >=',
	  round(score_min_Pareto*100, 2), '%')
# print('idx_good_train:', idx_good_train)

_names_good_train = [dict_statistics_df['train'].index[i] for i in idx_good_train]
_dict_strategies_convex_train = {e: dict_strategies[e] for e in _names_good_train}
_dict_strategies, _statistics_df, _nb_input = \
	utils.average_Pareto(_dict_strategies_convex_train, score_min_Pareto, _columns, 'Pareto_good')
print(_nb_input, 'strategies in', 'Pareto_good')
if _nb_input >= min_nb_input_quantiles:  # statistics based on enough input
	for _range in ['all', 'train', 'valid']:
		if not _statistics_df[_range].empty:
			dict_statistics_df[_range] = pd.concat(
				[dict_statistics_df[_range], _statistics_df[_range]], axis=0)
if _dict_strategies:  dict_strategies.update(_dict_strategies)



# average of averages
_dict_strategies_quantiles = {e: dict_strategies[e] for e in dict_strategies
							  if dict_strategies[e].source=='quantile'}
_dict_strategies, _statistics_df, _nb_input = \
	utils.average_Pareto(_dict_strategies_quantiles, score_min_Pareto, _columns, 'avg_of_avgs')
if _nb_input >= min_nb_input_quantiles:  # statistics based on enough input
	# # avg_of_avgs_of_avgs
	# # _names = names_strategies_selected.copy();	_names.remove('avg_of_avgs_of_avgs')
	# # list_allocs = [_dict_strategies[_name].allocation_df for _name in _names]
	# # _median = pd.concat(list_allocs, axis=1).median(axis=1)
	# # _alloc  = pd.concat(list_allocs + [_median], axis=1).mean(axis=1)
	# list_allocs = [_dict_strategies[_name].allocation_df
	# 			   for _name in ['decl_hi-avg_of_avgs', 'qrtl_hi-avg_of_avgs']]
	# _alloc  = pd.concat(list_allocs, axis=1).mean(axis=1)
	# _dict_strategies['avg_of_avgs_of_avgs'] = \
	# 	utils.Strategy('avg_of_avgs_of_avgs', 'quantile', None, None, allocation_df=_alloc)

	# ['decl_hi-avg_of_avgs',
	# 							 'qrtl_hi-avg_of_avgs',
	# 							 'median-avg_of_avgs',
	# 							 'avg_of_avgs_of_avgs']
	for _range in ['all', 'train', 'valid']:
		if not _statistics_df[_range].empty:
			dict_statistics_df[_range] = pd.concat(
				[dict_statistics_df[_range], _statistics_df[_range]], axis=0)
if _dict_strategies:  dict_strategies.update(_dict_strategies)
dict_sources = {_name: dict_strategies[_name].source for _name in dict_strategies}


_len = len(dict_statistics_df['train'].index)
# utils.Strategy.print_headings(range='valid')
# for i in idx_good_train:  # + [_len-6+e for e in range(0, 6)]:
# 	dict_strategies[dict_statistics_df['valid'].index[i]].print_one_line()


## Plot Pareto
# print('Starting Pareto plots...')
if len(idx_good_train) > 50: # enough in the convex envelope
	for _range in ['train', 'valid']:  # 'all',
		# print('Pareto front, convex hull, training [%]:\n', (100.*_statistics_train).round(2))
		_name = 'training' if _range == 'train' else ('validation' if _range == 'valid' else _range)
		plots.all4_vol_return(
				_statistics_all[_range], dict_statistics_df[_range].iloc[idx_good_train],
				dict_statistics_curve_df[_range],
				name_range=_name+'_with_Pareto_convex_high_score', ylim=(return_min_pc, return_max_pc),
				dict_sources=dict_sources)
else:  # plot the whole Pareto front
	for _range in ['train', 'valid']:  # 'all',
		# print('Pareto front, training [%]:\n', (100.*_statistics_train).round(2))
		_name = 'training' if _range == 'train' else ('validation' if _range == 'valid' else _range)
		plots.all4_vol_return(
			_statistics_all[_range],
			dict_statistics_df[_range].iloc[idx_efficient_train + [_len - 4 + e for e in range(0, 4)]],
			dict_statistics_curve_df[_range],
			name_range=_name + '_with_Pareto_front', ylim=(return_min_pc, return_max_pc),
			dict_sources=dict_sources)
# print('Pareto plots done.')

# plots.vol_return(
# 	dict_statistics_df		['train'].iloc[idx_convex_train]['score'],
# 	dict_statistics_curve_df['train']['score'],
# 	dict_statistics_df		['valid'].iloc[idx_convex_train]['score'],
# 	dict_statistics_curve_df['valid']['score'],
# 	'score training [%]', 'score validation [%]', 'score-train-valid',
# 	xlim=plots._range_score, ylim=plots._range_score, square=True, name_range=None,
# 	legend_loc='upper left', alpha=plots.alpha_Pareto, dict_sources=dict_sources)


## Plot time evolution
_strategies = {_name: dict_strategies[_name] for _name in
			   list(dict_statistics_df['all'].index[idx_good_train]) +
			   names_strategies_selected + ['alloc100_0', 'alloc70_30']}  # , 'alloc40_60'

_set_sources = {_strategies[e].source for e in _strategies}
# print('_set_sources:', _set_sources)
list_names_max_score = list()
for _source in _set_sources:
	# print(_source)
	if _source not in ['constant', 'quantile']:
		_max_score = -10
		_name_max_score = None
		for _name in _strategies:
			if _strategies[_name].source == _source: # or _name in names_strategies_selected:
				if _strategies[_name].score_policy['train'] > max(_max_score, score_min_Pareto):
					_max_score = _strategies[_name].score_policy['train']
					_name_max_score = _name
			# print(_name_max_score, _max_score)
		if _name_max_score is not None:
			list_names_max_score.append(_name_max_score)
# print(list_names_max_score)
# print('Validation statistics over `list_names_max_score`')
# print((100*dict_statistics_df['valid'].loc[list_names_max_score]).round(2))

list_names_max_score.sort(key=lambda _name: _strategies[_name].score_policy['train'], reverse=True)
print('list_names_max_score:', list_names_max_score)

# plots.time_evolution({_name: dict_strategies[_name]
# 		 for _name in list_names_max_score+['alloc100_0', 'alloc70_30', 'alloc40_60']})

_label_y = 'final'
_nfile   = 'final'
list_names_final = names_strategies_selected
# if len(list_names_max_score) <= 3: # adding too many would make for a messy figure
# 	list_names_final.extend(list_names_max_score)

# _label_y = 'best of each source'
# _nfile   = 'best_each_source'
# list_names_final = list_names_max_score

# plot allocation(time)
_alloc = pd.DataFrame.from_dict({_name: dict_strategies[_name].allocation_df
								 for _name in list_names_final})
plots.time_allocations		(_alloc.loc[range_train], _label_y+' (training)',   _nfile+'-train',  6)
plots.time_stdev_allocations(_alloc.loc[range_train], _label_y+' (training)',   _nfile+'-train',  6)
plots.time_slope_allocations(_alloc.loc[range_train], _label_y+' (training)',   _nfile+'-train', 3, 6)

plots.time_allocations		(_alloc.loc[range_valid], _label_y+' (validation)', _nfile+'-valid',  6)
plots.time_stdev_allocations(_alloc.loc[range_valid], _label_y+' (validation)', _nfile+'-valid',  6)
plots.time_slope_allocations(_alloc.loc[range_valid], _label_y+' (validation)', _nfile+'-valid', 3, 6)

plots.time_allocations		(_alloc.loc[range_recent],_label_y+' (recent)', 	_nfile+'-recent', 6)
plots.time_slope_allocations(_alloc.loc[range_recent],_label_y+' (recent)', 	_nfile+'-recent', 3, 6)

list_names_final.extend(['alloc100_0', 'alloc70_30']) # , 'alloc40_60'


# plot capital, returns and losses as functions of time
_TR_df = pd.DataFrame.from_dict({_name: dict_strategies[_name].TR_df
								 for _name in list_names_final})
plots.time_capital(_TR_df.loc[range_train], _label_y+' (training)', _nfile+'-train')
plots.cumul_dist({_name: dict_strategies[_name].returns_10yr for _name in _TR_df.columns},
				   'train', 'returns_10yr (training)', xlim=(-2.5, 17.5), ylim=(0., 100.))
plots.cumul_dist({_name: dict_strategies[_name].losses       for _name in _TR_df.columns},
				   'train', 'losses (training)', xlim=(0, 35.), ylim=(1., 100.), log=(False, True))
# plots.cumul_dist({_name: dict_strategies[_name].losses       for _name in _TR_df.columns},
# 				   'train', 'losses (training) log', xlim=(10, 50.), ylim=(0.1, 2.), log=(True, True))

_TR_df = pd.DataFrame.from_dict({_name: dict_strategies[_name].TR_df.loc[range_valid] /
										dict_strategies[_name].TR_df.loc[range_valid].iloc[0]
								 for _name in list_names_final})
plots.time_capital(_TR_df, _label_y+' (validation)', _nfile+'-valid')
plots.cumul_dist({_name: dict_strategies[_name].returns_10yr for _name in _TR_df.columns},
				   'valid', 'returns_10yr (validation)', xlim=(-2.5, 17.5), ylim=(0., 100.))
plots.cumul_dist({_name: dict_strategies[_name].losses       for _name in _TR_df.columns},
				   'valid', 'losses (validation)', xlim=(0, 35.), ylim=(1., 100.), log=(False, True))
# plots.cumul_dist({_name: dict_strategies[_name].losses       for _name in _TR_df.columns},
# 				   'valid', 'losses (validation) log', xlim=(10, 50.), ylim=(0.1, 2.), log=(True, True))
_recent_df = _TR_df.loc[_TR_df.index >= start_date_recent]
plots.time_capital(_recent_df.div(_recent_df.iloc[0]), _label_y+' (recent)', _nfile+'-recent')


# plot returns(time)
_return_gain_df = pd.DataFrame() #index=list_names_final)
_dict_ylim = {2: (-12, 28), 3: (-10, 25), 5: (-8, 23), 7: (-7, 22),
			  10: (-5, 20), 15: (-2, 18), 20: (-1, 15)}
_dict_do_plot = {_nb_years: False for _nb_years in _dict_ylim}
_dict_do_plot[5] = True;  _dict_do_plot[10] = True;  _dict_do_plot[20] = True

for _nb_years in dict_strategies[_name].dict_returns_df:
	_returns_df = pd.DataFrame.from_dict({_name: dict_strategies[_name].dict_returns_df[_nb_years]
								  for _name in list_names_final+['qrtl_low-avg_of_avgs']})
	_return_gain_df[_nb_years] = plots.time_return(_returns_df.loc[range_valid], _nb_years,
									_label_y+' (validation)', _nfile+'-valid', _nb_years/2,
									_dict_ylim[_nb_years], do_plot=_dict_do_plot[_nb_years])
# print(100 * _return_gain_df.T)
(100 * _return_gain_df.T).plot();  plt.xlim(1, 30);  plt.ylim(0, .4);  plt.semilogx()
plt.title('improvement over next best (validation) [% p.a.]'); plt.xlabel('length investment [years]')
plt.savefig(os.path.join('figures', "extra_return-valid.png"), format="png", dpi=200)
plt.close()


# Writing results to csv file
_df_for_csv = pd.DataFrame()


col_frames = []

for _name, _strategy in dict_strategies.items():
    row = [
        _increment if _strategy.source != 'constant' else None,
        _strategy.name,
        _strategy.source,
        _strategy.allocation_current,
    ]

    for _range in ['train', 'valid']:
        row.extend([
            _strategy.allocation_avg[_range],
            _strategy.avg_TR        [_range],
            _strategy.net_avg_TR_actual[_range],
            _strategy.turnover      [_range],
            _strategy.volatility    [_range],
            _strategy.vol_10yr      [_range],
            _strategy.cumul_losses  [_range],
            _strategy.max_loss      [_range],
            _strategy.score_actual  [_range],
        ])

    # build individual column as its own DataFrame
    col_frames.append(pd.DataFrame({_name: row}))

# concat once => prevents fragmentation warning
_df_for_csv = pd.concat(col_frames, axis=1)

# 	_list = [_increment if _strategy.source != 'constant' else None,
# 			 _strategy.name, _strategy.source, _strategy.allocation_current]
#
# 	for _range in ['train', 'valid']:
# 		_list.extend([_strategy.allocation_avg[_range], _strategy.avg_TR	  [_range],
# 					  _strategy.net_avg_TR_actual[_range],
# 					  _strategy.turnover	  [_range],
# 					  _strategy.volatility	  [_range], _strategy.vol_10yr	  [_range],
# 					  _strategy.cumul_losses  [_range], _strategy.max_loss	  [_range],
# 					  _strategy.score_actual[_range]])
# 	_df_for_csv[_name] = _list


_headings = ['increment', 'name', 'source', 'allocation_current']
for _range in ['train', 'valid']:
	_headings.extend(['allocation_avg-'+_range, 'avg_TR-'+_range, 'net_avg_TR_actual-'+_range,
					  'turnover-'+_range, 'volatility-'+_range, 'vol_10yr-'+_range,
					  'cumul_losses-'+_range, 'max_loss-'+_range, 'score_actual-'+_range])
_df_for_csv.index = _headings

_df_for_csv.T.to_csv('strategies.csv', index=False, sep=csv_sep)


# print('Validation statistics for', names_strategies_selected)
utils.Strategy.print_headings(range='valid')
for _name in names_strategies_selected:
	dict_strategies[_name].print_one_line('valid')

sys.exit()
