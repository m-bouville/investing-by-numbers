import os
import warnings
import datetime

from   sklearn.metrics import r2_score
# import scipy

import pandas  as pd
import numpy   as np

# import matplotlib.pyplot as plt



slope_TR_volatility  = {e: 0.54 / 2 for e in ['all', 'train', 'valid']}  # {'all': 0.23909, 'train': 0.23296, 'valid': 0.20054}
slope_TR_maxLoss 	 = {e: 0.21 / 2 for e in ['all', 'train', 'valid']}  # {'all': 0.05608, 'train': 0.05816, 'valid': 0.07321}
slope_TR_vol10yr 	 = {e: 0.27 / 2 for e in ['all', 'train', 'valid']}  # {'all': 0.17709, 'train': 0.16491, 'valid': 0.15579}
offset_score 	     = {e: 2.2/100  for e in ['all', 'train', 'valid']}  # {'all': 0.022047,'train': 0.017594,'valid': 0.032831}
# 1950:
# slope_TR_volatility  = {'all': 0.23909, 'train': 0.15875, 'valid': 0.19863}
# slope_TR_maxLoss 	 = {'all': 0.05608, 'train': 0.04085, 'valid': 0.07202}
# slope_TR_vol10yr 	 = {'all': 0.17709, 'train': 0.12952, 'valid': 0.14842}
# offset_score 	     = {'all': 0.02204, 'train': 0.02168, 'valid': 0.03157}

months_SMA = 12



# Classes
#########

class Prediction:
	def __init__(self, name, type_y, length, method, is_ln, prediction_df, R2_train, R2_valid):
		self.name				= name
		self.type_y				= type_y
		self.length				= length
		self.method				= method
		self.is_ln 				= is_ln
		self.prediction_df 		= prediction_df
		self.R2_train			= R2_train
		self.R2_valid			= R2_valid

		self.predict_current 	= prediction_df.iloc[-1]

	@classmethod
	def print_headings(cls):
		print('name     		       | type_y | length| method   |ln?|R² tr val | curr.')

	def print_one_line(self):
		if self.type_y == 'return':
			_format  = '{:.1f}%'
			_current = self.predict_current * 100.
		else:
			_format  = '{:.2f}'
			_current = self.predict_current

		_length_str = self.length.strip("avg_") if isinstance(self.length, str) else str(self.length).rjust(4)

		print(('{:23s}| {:7s}|{:7s}| {:9s}|{:3s}|{:3n}% {:3n}% | '+_format).
			  format(str(self.name)[:23],str(self.type_y), _length_str, str(self.method),
					 "ln" if self.is_ln else "  ", round(100*self.R2_train), round(100*self.R2_valid), _current))

	def print(self):
		print('`name`     		: {:s}'.format(self.name))
		print('`y`        		: {:s}'.format(self.y))
		print('`type`     		: {:s}'.format(self.type))
		print('`is_ln`    		: {}'  .format(self.is_ln))
		print('`R2train` 		: {:.1f}%'.format(100*self.R2_train))
		print('`predict_current`: {:.3f}' .format(self.predict_current))




class Strategy:
	duration_alloc	 = 0.
	duration_alloc_DD= 0.
	duration_TR  	 = 0.

	def __init__(self, name, source,
				 allocation_amplitude_ref, allocation_target,
				 allocation_df=None, TR_df=None, range='all'):
		self.name				= name
		self.allocation_target 	= allocation_target
		self.source				= source
		self.avg_TR    	= dict();	self.net_avg_TR_policy=dict();	self.net_avg_TR_actual=dict()
		self.turnover  	= dict(); 	self.volatility		= dict()
		self.allocation_avg=dict(); self.allocation_stdev=dict()
		self.cumul_losses=dict();  	self.avg_loss		= dict();  self.losses		   = dict()
		self.max_loss  	= dict(); 	self.max_loss_date	= dict()
		self.vol_10yr 	= dict(); 	self.returns_10yr   = dict();  self.decl_low_returns= dict()

		if source == 'constant':
			self.allocation_df 			= None
			self.allocation_amplitude 	= allocation_amplitude_ref
			self.allocation_avg['all']  = allocation_target;  self.allocation_stdev['all']  = 0.
			self.allocation_avg['train']= allocation_target;  self.allocation_stdev['train']= 0.
			self.allocation_avg['valid']= allocation_target;  self.allocation_stdev['valid']= 0.
			self.allocation_current 	= allocation_target

			self.TR_df = TR_from_constant_allocation(allocation_target, name, all_data.index)

		else:
			_start_alloc = duration_to_now()  # timing the code
			if source in ['meta3', 'avg', 'min', 'max', 'quantile']:
				self.allocation_df 		= allocation_df
				self.allocation_amplitude=allocation_amplitude_ref
			else:
				if allocation_df is None:
					self.__allocation_from_predictions(dict_predictions[self.source], allocation_amplitude_ref)
				else:
					self.allocation_df = allocation_df

			for _name_range in ['all', 'train', 'valid']:
				_alloc = self.allocation_df.loc[dict_ranges[_name_range]]
				self.allocation_avg  [_name_range] = _alloc.mean(axis=0)
				self.allocation_stdev[_name_range] = _alloc.std (axis=0)
			self.allocation_current     = self.allocation_df.iloc[-1]

			Strategy.duration_alloc, _start_TR = duration_to_now(_start_alloc, Strategy.duration_alloc)  # timing the code

			if TR_df is None:
				self.TR_df = TR_from_allocation(self.allocation_df, self.name)
			else:
				self.TR_df = TR_df

			Strategy.duration_TR, _ = duration_to_now(_start_TR, Strategy.duration_TR)  # timing the code

		self.dict_returns_df = dict()
		for _nb_years in [2, 3, 5, 7, 10, 15, 20]:
			self.dict_returns_df[_nb_years]  = self.TR_df.div(self.TR_df.shift(_nb_years*12)) ** (1/_nb_years) - 1

		# if self.allocation_df is not None:
		# 	print(100*self.allocation_df.min(axis=0), 100*self.allocation_df.max(axis=0))

		_allocation_train = self.allocation_df.loc[range_train] \
			if self.allocation_df is not None else None

		_dict_TR_df = {'all': self.TR_df, 'train': self.TR_df.loc[range_train],
					   'valid': self.TR_df.loc[range_valid]}
		for _name_range in _dict_TR_df:
			(self.avg_TR	 [_name_range], self.volatility[_name_range], self.cumul_losses [_name_range],
				self.avg_loss[_name_range], self.max_loss  [_name_range], self.max_loss_date[_name_range],
				self.losses	 [_name_range], self.vol_10yr  [_name_range], self.returns_10yr [_name_range],
				self.decl_low_returns[_name_range])  = statistics_one(_dict_TR_df[_name_range])


		if self.allocation_df is not None:
			for _range in ['all', 'train', 'valid']:
				self.turnover[_range]= abs(self.allocation_df.loc[dict_ranges[_range]] -
										   self.allocation_df.loc[dict_ranges[_range]].shift(1)).\
				squeeze().iloc[1:].mean() * 12.
		else:
			for _range in ['all', 'train', 'valid']:	self.turnover[_range]   = 0

		# self.median_return_10yr = dict()
		# self.sd_return_10yr 	= dict();  self.quartile_low_return_10yr = dict()
		# if self.TR_df is not None:
		# 	_return_10yr_df = (self.TR_df / self.TR_df.shift(10*12)).squeeze() ** .1 - 1.
		# 	# plt.plot(_return_10yr_df*100); plt.show()
		# 	for _name_range in ['all', 'train', 'valid']:
		# 		_range = dict_ranges[_name_range]
		# 		self.median_return_10yr 	 [_name_range] = _return_10yr_df.loc[_range].median()
		# 		self.sd_return_10yr 		 [_name_range] = _return_10yr_df.loc[_range].std()
		# 		self.quartile_low_return_10yr[_name_range] = _return_10yr_df.loc[_range].quantile(q=0.25)
		# 		# print(name, _name_range, round(100*self.median_return_10yr[_name_range], 2),
		# 		# 	  round(100*self.sd_return_10yr[_name_range], 2),
		# 		# 	  round(100*self.quartile_low_return_10yr[_name_range], 2))
		# else:
		# 	for _name_range in ['all', 'train', 'valid']:
		# 		self.median_return_10yr	 	 [_name_range] = None
		# 		self.sd_return_10yr  		 [_name_range] = None
		# 		self.quartile_low_return_10yr[_name_range] = None

		self.score_policy = dict();  self.score_actual = dict()
		for _name_range in ['all', 'train', 'valid']:
			self.net_avg_TR_policy[_name_range] = \
				self.avg_TR[_name_range] - trading_cost_policy * self.turnover[_name_range]
			self.net_avg_TR_actual[_name_range] = \
				self.avg_TR[_name_range] - trading_cost_actual * self.turnover[_name_range]

			_corrections = slope_TR_volatility[_name_range] * 2/3 * self.volatility[_name_range] \
						 + slope_TR_maxLoss   [_name_range] / 6   * self.max_loss  [_name_range] \
						 + slope_TR_vol10yr   [_name_range] / 6   * self.vol_10yr  [_name_range] + offset_score[_name_range]
			# - slope_decl_low_returns[_name_range] / 6 * self.decl_low_returns[_name_range]
			# - slope_TR_quartileReturn[_name_range]  /3 * self.quartile_low_return_10yr[_name_range] \
			self.score_policy[_name_range] = self.net_avg_TR_policy[_name_range] - _corrections
			self.score_actual[_name_range] = self.net_avg_TR_actual[_name_range] - _corrections
		# print(self.name, round_dict(self.score, 2, 100))

		# self.print_one_line(range=range)
		# print(self.allocation_df)

	def __allocation_from_predictions(self, predictions, allocation_amplitude_ref):
		_type_prediction = predictions.type_y
		_prediction_df   = predictions.prediction_df
		if _type_prediction == 'return':
			self.allocation_amplitude = allocation_amplitude_ref
			# print(name, _type_prediction, self.allocation_amplitude, self.allocation_target)
			self.allocation_df = allocation_from_return(
				_prediction_df, target=self.allocation_target, amplitude=self.allocation_amplitude)
		elif 'time_since_max' in _type_prediction:
			self.allocation_amplitude = -allocation_amplitude_ref / 50.
			self.allocation_df = allocation_from_time_since_max(
				_prediction_df, target=self.allocation_target, amplitude=self.allocation_amplitude)
		elif 'TR_' in _type_prediction:
			if 'TR_min' in _type_prediction:
				self.allocation_amplitude = allocation_amplitude_ref / 12. / np.log(2.5)
				self.allocation_target -= 0.05
			else:
				self.allocation_amplitude = allocation_amplitude_ref / 9. / np.log(2.5)

			_start_alloc_DD = duration_to_now()  # timing the code
			_predict = _prediction_df if predictions.is_ln else np.log(_prediction_df)
			self.allocation_df = allocation_from_ln_TR_ratio(
				_predict, target=self.allocation_target, amplitude=self.allocation_amplitude,
				fraction_zigzag=0.5)
			Strategy.duration_alloc_DD, _ = \
				duration_to_now(_start_alloc_DD, Strategy.duration_alloc_DD)  # timing the code

		else:
			raise NotImplementedError(_type_prediction)


	def split_strategy(self):
		_dict = dict()
		for _name_range in ['all', 'train', 'valid']:
			_dict[_name_range] = [
				self.net_avg_TR_policy[_name_range],self.net_avg_TR_actual[_name_range],
				self.volatility	    [_name_range],
				self.avg_loss  		[_name_range], 	self.max_loss	 [_name_range], self.max_loss_date [_name_range],
				self.vol_10yr		[_name_range],
				self.allocation_avg	[_name_range],	self.turnover	 [_name_range],
				# self.median_return_10yr[_name_range],self.sd_return_10yr[_name_range],
				# self.quartile_low_return_10yr[_name_range],
				self.score_policy   [_name_range],  self.score_actual   [_name_range],
				self.allocation_current if _name_range != 'train' else None]
		return _dict['all'], _dict['train'], _dict['valid']

	# def plot_time_TR(self, range_years, months_SMA=plots.months_SMA_plots):
	# 	plots.time_TR(self.TR_df, self.name, [min(range_years), max(range_years)], months_SMA=months_SMA)


	## Printing
	def print_headings(range='all'):
		print(' input parameters         |        results: ' + range)
		print('   name     | allocation  | net |turn|volatility |   losses   |decl.|')
		print('            | avg cur diff|rtrn |over|      10yr | avg. max yr|rtrn.|score')
	def print_line():
		print('------------+-------------+-----+----+-----------+------------+-----+-----')

	def print_one_line(self, range='all'):
		_amplitude = '{:5.1f}'.format(self.allocation_amplitude) \
			if self.allocation_amplitude is not None else ' None'
		_target = '{:3n}%'.format(round(100*self.allocation_target))\
			if self.allocation_target    is not None else 'None'
		_str = '{:12s}|{:3n}%{:3n}%{:+4n}%|{:4.1f}%|{:3n}%|'. \
			  format(self.name[:12],
					 round(100*self.allocation_avg[range]), round(100*self.allocation_current),
					 round(100 * (self.allocation_current - self.allocation_avg[range])),
					 100*self.net_avg_TR_actual[range], round(100*self.turnover[range]))
		_str += ('{:4.1f}%{:5.1f}%|{:4.1f}%{:3n}% {:s}|{:4.1f}%|{:4.1f}%'). \
			  format(100*self.volatility[range], 100*self.vol_10yr[range],
					 100*self.cumul_losses[range], round(100*self.max_loss[range]),
					 str(round(self.max_loss_date[range]))[-2:], 100*self.decl_low_returns[range],
					 100*self.score_actual[range])
		print(_str)

	def print(self):
		print('`name`               : {:s}'	  .format(self.name))
		print('`source`             : {:s}'	  .format(self.source))
		print('`allocation_amplitude`:{}'	  .format(self.allocation_amplitude))
		print('`allocation_target`  : {:.1f}%'.format(100*self.allocation_target))
		print('`allocation_avg`     : {:.1f}%'.format(100*self.allocation_avg))
		print('`allocation_current` : {:.1f}%'.format(100*self.allocation_current))





# Utilities
###########

def import_data(nfile='SP500.csv', index_col='date', sep=";", decimal="."):
	all_data = pd.read_csv(nfile, index_col=index_col, sep=sep, decimal=decimal) #, dtype='float64')  # , na_values='#N/A')

	all_data['ln_TR_min']     	 	= np.log(all_data['TR_min'])
	all_data['ln_TR_max']     	 	= np.log(1 + all_data['TR_max'])
	all_data['ln_TR_max_off5yr'] 	= np.log(1 + all_data['TR_max_off5yr'])
	all_data['ln_TR_max_off_m5yr']	= np.log(1 + all_data['TR_max_off_m5yr'])
	all_data['inv_TR_ratio']  		= 1 / all_data['TR_ratio']
	all_data['inv_TR_ratio_1yr']	= 1 / all_data['future_TR_ratio_1yr']
	all_data['inv_TR_ratio_2yr']	= 1 / all_data['future_TR_ratio_2yr']
	all_data['inv_TR_ratio_5yr']	= 1 / all_data['future_TR_ratio_5yr']
	all_data['-ln_TR_ratio']     	=-np.log(all_data['TR_ratio'])
	all_data['ln_divi_yield'] 	 	= np.log(all_data['dividend_yield'] * 100.)

	all_data.drop(columns=['TR_ratio', 'future_TR_ratio_1yr', 'future_TR_ratio_2yr',
						   'future_TR_ratio_5yr'], inplace=True) # counterintuitive

	return all_data


def name_png(length):
	if 'avg' in str(length):
		return length
	if 'TR_' in str(length) or length in ['meta', 'time_since_max']:
		return length
	return str(length) + 'yr'

def name_y_df(length):
	if 'TR_' in str(length) or length in ['time_since_max']:
		return length
	else:
		return 'future_real_' + name_y(length)

def name_y(length):
	if 'TR_' in str(length) or length in ['time_since_max']:
		return length
	if length in ['meta', 'meta2'] or 'avg' in str(length):
		return 'return_' + length
	return 'return_' + str(length) + 'yr'

def trimmed_name(name):
	if '-lin+rf' in name or '-linear' in name: return name[:-7]
	if '-rf' in name: return name[:-3]
	return name

def round_dict(dictionary, ndigits=0, multiplier=1.):
	return {x: round(multiplier*dictionary[x], ndigits) for x in dictionary}

def round_dict_dict(dictionary, ndigits=0, multiplier=1.):
	return {x: round_dict(dictionary[x], ndigits, multiplier) for x in dictionary}


## timing
def duration_to_now(start_time=None, t0=0.):
    '''
    time the execution of the code.

    Parameters
    ----------
    start_time : datetime.datetime, default None
        time when the execution started (None: first use)
    t0 : float, default 0.
        cumulative time [s] so far

    Returns
    -------
    datetime.datetime or (float, datetime.datetime)
        if start_time is None (first use) return the current time
        else return (cumulative_time, current_time)
    '''
    if start_time is None:
        return datetime.datetime.now()
    else:
        return t0 + (datetime.datetime.now() - start_time) / pd.Timedelta(seconds=1), datetime.datetime.now()



## allocation from predictions
##############################

def allocation_from_return(prediction_return, target, amplitude,
						   moving_average=12, lower=0., upper=1., fillna=0.5):
	output = target + amplitude * \
		(prediction_return.rolling(moving_average, center=False, min_periods=int(moving_average/2)).mean() -
	 		prediction_return.mean())

	output.fillna(fillna, inplace=True)
	return output.clip(lower=lower, upper=upper)

def allocation_from_ln_TR_ratio(prediction_ln_TR_ratio, target,	amplitude,
								lower=0., upper=1., fraction_zigzag=0.5):  #, slope_max=.5/12  # curv_max=.05
	# _pred = prediction_ln_TR_ratio.clip(lower=0, upper=np.log(2.5)) / np.log(2.5)  # now in [0,1]
	_raw_target = target + amplitude * \
				  (prediction_ln_TR_ratio - prediction_ln_TR_ratio.mean())

	output = pd.Series(index=_raw_target.index, dtype='float64')
	output.iloc[0] = _raw_target.iloc[0];   output.iloc[1] = _raw_target.iloc[1]
	output.iloc[2] = _raw_target.iloc[2];   output.iloc[3] = _raw_target.iloc[3]

	if fraction_zigzag == 1.:
		return _raw_target.clip(lower=lower, upper=upper)

	for _t in range(4, len(output.index)):
		if (_raw_target.iloc[_t  ] - output.iloc[_t-2]) * \
		   (output	   .iloc[_t-2] - output.iloc[_t-4]) >= 0:  # same sign => no zigzag
			_output = _raw_target.iloc[_t]
		else: # zag limited to fraction of zig
			_output = output.iloc[_t-2] + \
					  (_raw_target.iloc[_t] - output.iloc[_t-2]) * fraction_zigzag

		# _curv = output.iloc[_t-2] + _raw_target.iloc[_t] - 2.*output.iloc[_t-1]
		#
		# print(_t, _raw_target.iloc[_t], output.iloc[_t-1],
		# 	  _raw_target.iloc[_t  ] - output.iloc[_t-1],
		# 	  output	  .iloc[_t-1] - output.iloc[_t-2])
		# if abs(_curv) < curv_max:  # no zigzag
		# 	_output = _raw_target.iloc[_t]
		# else:
		# 	_slope  = _raw_target.iloc[_t] - output.iloc[_t-1]
		# 	_output = 2. * output.iloc[_t - 1] - output.iloc[_t - 2] + curv_max * np.sign(_curv)
		output.iloc[_t] = _output
		# output.iloc[_t] = max(lower, min(upper, _raw_target.iloc[_t] if abs(_slope) < slope_max \
		# 	else output.iloc[_t-1] + slope_max * np.sign(_slope)))
		# print(_t, _curv, _raw_target.iloc[_t], output.iloc[_t])

	# plt.plot(100. * pd.concat([_raw_target, output], axis=1)); plt.show()

	return output.clip(lower=lower, upper=upper)

def allocation_from_time_since_max(prediction_time_since_max, target, amplitude,
								   moving_average=1, lower=0., upper=1.):
	output = target + amplitude * (prediction_time_since_max.rolling(
					moving_average, center=False, min_periods=int(moving_average/2)).mean()-
			  prediction_time_since_max.mean())
	return output.clip(lower=lower, upper=upper)



## TR_from_allocation
#####################

def TR_from_allocation(allocation, name):
	output = pd.Series(name=name, index=allocation.index, dtype='float64')
	output.iloc[0] = 1.  # real_TR_valid.iloc[0]
	# print(output.shape, allocation.shape, ratio_SP_valid.shape, ratio_obli_valid.shape)

	_alloc_prev = allocation.squeeze().shift(1)
	_variation = _alloc_prev * ratio_SP.squeeze() + (1-_alloc_prev) * (1+ratio_obli.squeeze())
	# print(allocation); print(_alloc_prev); print(ratio_SP.squeeze()); print(ratio_obli.squeeze()); print(_variation)
	# print(pd.concat([allocation, _alloc_prev, ratio_SP.squeeze(), ratio_obli.squeeze(), _variation.squeeze()], axis=1))

	# for t in range(1, len(allocation.index)):
	# 	# print(t, output.iloc[t-1], allocation.iloc[t-1], ratio_SP_valid  .iloc[t], ratio_obli_valid.iloc[t])
	# 	output.iloc[t] = output.iloc[t-1] * _variation.iloc[t]
	# 	# output.iloc[t] = output.iloc[t-1] * (  allocation.iloc[t-1]  *    ratio_SP  .iloc[t] +
	# 	# 									(1-allocation.iloc[t-1]) * (1+ratio_obli.iloc[t]))

	return _variation.cumprod(skipna=True)

def TR_from_constant_allocation(allocation, name, index):
	output = pd.Series(name=name, index=index, dtype='float64')
	output.iloc[0] = 1.  # real_TR_valid.iloc[0]
	_variation = allocation * ratio_SP.squeeze() + (1-allocation) * (1+ratio_obli.squeeze())
	# for t in range(1, len(index)):
	# 	output.iloc[t] = output.iloc[t-1] * _variation.iloc[t]
	# 	# output.iloc[t] = output.iloc[t - 1] * (allocation * ratio_SP.iloc[t] +
	# 	# 								   (1 - allocation) * (1 + ratio_obli.iloc[t]))
	return _variation.cumprod(skipna=True)

def prediction_meta_one_y(df_predictions, name_y, dict_weights=None):
	if dict_weights is not None:
		assert set(df_predictions.columns) == set(dict_weights.keys()), \
			'df_predictions.columns {} != dict_weights.keys() ({})'.\
				format(set(df_predictions.columns), set(dict_weights.keys()))
	else:
		dict_weights = {_name: 1 for _name in df_predictions.columns}
	sum_weights = sum([dict_weights[x] for x in dict_weights])

	_df_pred     = pd.DataFrame(index=df_predictions.index)
	_df_all_data = all_data[name_y]

	for _name in df_predictions.columns:
		_df_pred[_name] = dict_weights[_name] / sum_weights * df_predictions[_name]

	_prediction_meta  = _df_pred	.sum(axis=1, skipna=False)

	# print(_df_all_data_meta.loc[clean_all_data.index])
	# print(_prediction_meta   .loc[clean_all_data.index])

	_R2_train = r2_score(_df_all_data.loc[range_train],       _prediction_meta.loc[range_train])
	_R2_valid = r2_score(_df_all_data.loc[range_clean_valid], _prediction_meta.loc[range_clean_valid])
	_predict_current_meta = round(_prediction_meta.iloc[-1], 5)

	return _prediction_meta, _R2_train, _R2_valid, _predict_current_meta

def prediction_meta_several_y(df_predictions, dict_weights=None):
	"""
	If `dict_weights` is None: simple average of predictions. Else: weighted average.
	Warning: if the method is called several times with different weights,
	the variable being predicted will be different.
	:param df_predictions: dict of Predictions
	:param dict_weights: dict of floats or None, default None
	:return: Tuple[Prediction, float, float]
	"""
	if dict_weights is not None:
		assert set(df_predictions.columns) == set(dict_weights.keys())
		_dict_weights = dict_weights
	else:
		_dict_weights = {length: 1 for length in df_predictions.columns}
	_sum_weights = sum([_dict_weights[x] for x in _dict_weights])

	_df_pred     = pd.DataFrame(index=df_predictions.index)
	_df_all_data = pd.DataFrame(index=all_data      .index)

	for length in df_predictions.columns:
		_name_y = name_y(length)
		_df_pred  	[length] = _dict_weights[length] / _sum_weights * df_predictions[length]
		_df_all_data[length] = _dict_weights[length] / _sum_weights * all_data      [_name_y]

	_prediction_meta  = _df_pred	.sum(axis=1, skipna=False)
	_df_all_data_meta = _df_all_data.sum(axis=1, skipna=False)

	# print(_df_all_data_meta.loc[clean_all_data.index])
	# print(_prediction_meta   .loc[clean_all_data.index])

	_R2 = round(r2_score(_df_all_data_meta.loc[range_train],
						  _prediction_meta.loc[range_train]), 4)

	return _prediction_meta, _R2


def prediction_meta2(df_predictions_1, df_predictions_2, dict_R2_1, dict_R2_2):
	assert set(df_predictions_1.columns) == set(df_predictions_2.columns) == \
		   set(dict_R2_1.keys()) == set(dict_R2_2.keys())
	sum_sqr_R2 = sum([dict_R2_1[x] ** 2 for x in dict_R2_1]) + \
				 sum([dict_R2_2[x] ** 2 for x in dict_R2_2])

	_df_pred       = pd.DataFrame(index=df_predictions_1.index)
	# _df_all_data   = pd.DataFrame(index=all_data		.index)
	_df_all_data = pd.DataFrame(index=all_data	.index)

	for length in df_predictions_1.columns:
		_name_y = name_y_df(length)

		_df_pred  	 [length] = \
			dict_R2_1[length] ** 2 / sum_sqr_R2 * df_predictions_1[length] + \
			dict_R2_2[length] ** 2 / sum_sqr_R2 * df_predictions_2[length]
		_df_all_data[length] = (dict_R2_1[length] ** 2 + dict_R2_2[length] ** 2) / sum_sqr_R2 * \
								 all_data   [_name_y]
		# _df_all_data  [length] = (dict_R2_1[length] ** 2 + dict_R2_2[length] ** 2) / sum_sqr_R2 * \
		# 						 all_data   [_name_y]

	_prediction_meta					  = _df_pred	.sum(axis=1, skipna=False)
	all_data  ['future_real_return_meta2']= _df_all_data.sum(axis=1, skipna=False)
	valid_data['future_real_return_meta2']= all_data['future_real_return_meta2'].loc[range_valid]

	# print(_df_valid_data_meta.loc[clean_valid_data.index])
	# print(_prediction_meta.loc[clean_valid_data.index])

	_R2 = round(r2_score(all_data  ['future_real_return_meta2'].loc[range_train],
						 _prediction_meta					   .loc[range_train]), 4)
	_predict_current_meta = round(_prediction_meta.iloc[-1], 5)

	return _prediction_meta, _R2, _predict_current_meta


# statistics
############

# calculating slopes for score
def slopes_for_score(dict_strategies):
	slope_TR_volatility 	= dict();   slope_TR_maxLoss 	= dict()
	slope_TR_vol10yr	  	= dict();   slope_decl_low_returns = dict()
	slope_TR_quartileReturn = dict()
	for _range in ['all', 'train', 'valid']:
		_delta_net_TR   	  = dict_strategies['alloc80_20'].net_avg_TR_actual[_range] \
							  - dict_strategies['alloc60_40'].net_avg_TR_actual[_range]
		_delta_volatility	  = dict_strategies['alloc80_20'].volatility	  [_range] \
						  	  - dict_strategies['alloc60_40'].volatility	  [_range]
		_delta_max_loss 	  = dict_strategies['alloc80_20'].max_loss  	  [_range] \
						  	  - dict_strategies['alloc60_40'].max_loss  	  [_range]
		_delta_vol_10yr 	  = dict_strategies['alloc80_20'].vol_10yr  	  [_range] \
						  	  - dict_strategies['alloc60_40'].vol_10yr  	  [_range]
		_delta_decl_low_returns=dict_strategies['alloc80_20'].decl_low_returns[_range] \
						      - dict_strategies['alloc60_40'].decl_low_returns[_range]

		# _delta_quartile_low_return_10yr = dict_strategies['alloc80_20'].quartile_low_return_10yr[_range] - \
		# 				  				  dict_strategies['alloc60_40'].quartile_low_return_10yr[_range]
		slope_TR_volatility	[_range] = _delta_net_TR / _delta_volatility
		slope_TR_maxLoss   	[_range] = _delta_net_TR / _delta_max_loss
		slope_TR_vol10yr	[_range] = _delta_net_TR / _delta_vol_10yr
		slope_decl_low_returns[_range]=_delta_net_TR / _delta_decl_low_returns
		# slope_TR_quartileReturn[_range] = _delta_net_TR / _delta_quartile_low_return_10yr
	print('slopes:')
	print('volatility:        ', round_dict(slope_TR_volatility, 5))
	print('max loss:          ', round_dict(slope_TR_maxLoss, 5))
	print('vol 10 yr:         ', round_dict(slope_TR_vol10yr, 5))
	print('decile low returns:', round_dict(slope_decl_low_returns, 5))
	# print(utils.slope_TR_quartileReturn)


def statistics_one(TR):
	_index 		= ~TR.isna()
	model_exp 	= np.polyfit(TR.index[_index], np.log(TR[_index]), 1)
	avg_TR 		= round(model_exp[0], 4)
	_detrended  = np.log(TR[_index]) - (TR.index[_index] * model_exp[0] + model_exp[1])
	volatility	= (_detrended - _detrended.shift(12)).dropna().std()

	# volatility 	= (TR.div(TR.shift(    1)).squeeze()).rolling(window=months_SMA).std().mean() * np.sqrt(12.)
	returns_10yr= TR.div(TR.shift(10*12)).squeeze().dropna()
	vol_10yr 	= returns_10yr.rolling(window=months_SMA).std().mean() * np.sqrt(12./10.)
		# average of rolling annual std. deviation
	returns_10yr = sorted((returns_10yr ** (1./10) - 1))
	decl_low_returns = returns_10yr[int(len(returns_10yr)/10)]
	# range_returns = -scipy.stats.linregress([1-e/_length for e in range(_length)], returns_10yr)[0]

	cumul_losses = (1 - TR.div(TR.shift( 1)).squeeze().iloc[ 1:]).clip(lower=0.).mean() * 12
	_loss 		 = (1 - TR.div(TR.rolling(36).max().squeeze()))  .clip(lower=0.)
	avg_loss = _loss.mean();  max_loss = _loss.max();  max_loss_date = _loss.idxmax()
	# max_loss   = (1 - TR.div(TR.shift(12)).squeeze().iloc[12:]).clip(lower=0.).max() # TODO real min

	return(avg_TR, volatility, cumul_losses, avg_loss, max_loss, max_loss_date,
		   sorted(_loss), vol_10yr, returns_10yr, decl_low_returns)

# def statistics(TR, allocation, trading_cost, verbose=0):
# 	avg_TR 	 = dict(); volatility 	 = dict(); cumul_losses = dict();  avg_loss 	= dict()
# 	max_loss = dict(); max_loss_date = dict(); vol_10yr 	= dict()
# 	returns_10yr = dict();  _decl_low_returns = dict()
# 	for name in TR.columns:
# 		(avg_TR[name], volatility[name], cumul_losses[name], avg_loss[name], max_loss[name], max_loss_date[name],
# 			_, vol_10yr[name], returns_10yr[name], _decl_low_returns[name]) = statistics_one(TR[name])
# 		if allocation is not None:
# 			turnover[name] = abs(allocation[name]-allocation[name].shift(1)).squeeze().iloc[1:].mean()*12.
#
# 	net_avg_TR = {e: avg_TR[e] - trading_cost*turnover[e] for e in avg_TR}
#
# 	if verbose > 0:
# 		print(pd.DataFrame.from_dict(round_dict_dict(
# 			{'real_returns': avg_TR,
# 			 'net_real_ret': net_avg_TR,
# 			 'volatility': 	 volatility,
# 			 'cumul_losses': cumul_losses,
# 			 'turnover': 	 turnover}, 	1, 100.)))
#
# 	return(avg_TR, net_avg_TR, volatility, cumul_losses, turnover)

def statistics_curve(TR):
	avg_TR 	 = dict(); volatility 	 = dict(); cumul_losses = dict();  avg_loss 	= dict()
	max_loss = dict(); max_loss_date = dict(); vol_10yr 	= dict();
	returns_10yr = dict();  _decl_low_returns = dict()
	for name in TR.columns:
		(avg_TR[name], volatility[name], cumul_losses[name], avg_loss[name], max_loss[name], max_loss_date[name],
			_, vol_10yr[name], returns_10yr[name], _decl_low_returns[name]) = statistics_one(TR[name])

	return(avg_TR, volatility, cumul_losses)


# Pareto
########

# Fairly fast for many datapoints, less fast for many costs, somewhat readable
# source: https://stackoverflow.com/questions/32791911/fast-calculation-of-pareto-front-in-python
def is_pareto_efficient_simple(costs):
	"""
    Find the pareto-efficient points
    :param costs: An (n_points, n_costs) array
    :return: A (n_points, ) boolean array, indicating whether each point is Pareto efficient
    """
	is_efficient = np.ones(costs.shape[0], dtype = bool)
	for i, c in enumerate(costs):
		# print(i, c)
		if is_efficient[i]:
			is_efficient[is_efficient] = np.any(costs[is_efficient] < c, axis=1)  # Keep any point with a lower cost
			is_efficient[i] = True  # And keep self
	return is_efficient


# Faster than is_pareto_efficient_simple, but less readable.
# source: https://stackoverflow.com/questions/32791911/fast-calculation-of-pareto-front-in-python
def is_pareto_efficient(costs, return_mask = True):
    """
    Find the pareto-efficient points
    :param costs: An (n_points, n_costs) array
    :param return_mask: True to return a mask
    :return: An array of indices of pareto-efficient points.
        If return_mask is True, this will be an (n_points, ) boolean array
        Otherwise it will be a (n_efficient_points, ) integer array of indices.
    """
    is_efficient = np.arange(costs.shape[0])
    n_points = costs.shape[0]
    next_point_index = 0  # Next index in the is_efficient array to search for

    while next_point_index<len(costs):
        nondominated_point_mask = np.any(costs<costs[next_point_index], axis=1)
        nondominated_point_mask[next_point_index] = True
        is_efficient = is_efficient[nondominated_point_mask]  # Remove dominated points
        costs = costs[nondominated_point_mask]
        next_point_index = np.sum(nondominated_point_mask[:next_point_index])+1

    if return_mask:
        is_efficient_mask = np.zeros(n_points, dtype = bool)
        is_efficient_mask[is_efficient] = True
        return is_efficient_mask
    else:
        return is_efficient



def average_Pareto(dict_strategies, score_min_Pareto, names_columns, name_Pareto='Pareto'):
	# TODO call with Pareto front data only
	_list_names = [_name for _name in dict_strategies
				  if (dict_strategies[_name].allocation_df is not None and
					  dict_strategies[_name].score_policy['train'] >= score_min_Pareto)]
	if len(_list_names) == 0:
		warnings.warn('no strategy with a score >= {}% for `{:s}`'.
					  format(score_min_Pareto*100, name_Pareto))
		return(dict(), dict(), 0)


	_dict_strategies = dict()
	_statistics_df = {'all': pd.DataFrame(), 'train': pd.DataFrame(), 'valid': pd.DataFrame()}
	_list_names_strategies = [_name for _name in dict_strategies
							  if dict_strategies[_name].allocation_df is not None]

	## avg, min and max Pareto
	# _weight_total = [dict_strategies[_name].score['train'] ** 2 for _name in _list_names_strategies]
	# # print(_weight_total)
	# _weight_total = sum(_weight_total)
	# _allocations_weighted = pd.concat([dict_strategies[_name].allocation_df *
	# 								   dict_strategies[_name].score['train'] ** 2
	# 								   for _name in _list_names_strategies], axis=1) / _weight_total
	# print('_allocations_Pareto:', _allocations_Pareto)

	# print('idx_efficient_train:', idx_efficient_train)
	# print('scores:', [dict_strategies[_name].score['train']  for _name in dict_strategies])
	_allocations_threshold = pd.concat(
		[dict_strategies[_name].allocation_df for _name in _list_names_strategies
		 if dict_strategies[_name].score_policy['train'] >= score_min_Pareto], axis=1)
	# print('_allocations_threshold:', _allocations_threshold)

	def do_strategy(prefix, quantile):
		_name = prefix + '-' + name_Pareto
		_dict_strategies[_name] = Strategy(_name, 'quantile', None, None, 
					allocation_df=_allocations_threshold.quantile(q=quantile, axis=1))

	_dict_quantile = {'qrtl_low': 0.25, 'median': 0.50, 'qrtl_hi': 0.75}
	
	for _prefix in _dict_quantile:
		do_strategy(_prefix, _dict_quantile[_prefix])
		
	# _name = 'min-' + name_Pareto;   _dict_strategies[_name] = Strategy(
	# 	_name, 'quantile', None, None, allocation_df=_allocations_threshold.min(axis=1))
	_name = 'avg-' 	+ name_Pareto;   _dict_strategies[_name] = Strategy(
		_name, 'quantile', None, None, allocation_df=_allocations_threshold.mean(axis=1))

	# do deciles only if there are enough data
	if len(_list_names) > 20:
		do_strategy('decl_low', 0.1)
		do_strategy('decl_hi',  0.9)
		for _name in ['decl_low-' + name_Pareto, 'decl_low-' + name_Pareto]:
			_statistics_df['all'  ][_name], _statistics_df['train'][_name], \
			_statistics_df['valid'][_name] = _dict_strategies[_name].split_strategy()


	_qrtl_low= _dict_strategies['qrtl_low-'+ name_Pareto].allocation_df
	_median  = _dict_strategies['median-'  + name_Pareto].allocation_df
	_qrtl_hi = _dict_strategies['qrtl_hi-' + name_Pareto].allocation_df
	if len(_list_names) > 20:  _decl_hi = _dict_strategies['decl_hi-' + name_Pareto].allocation_df

	_qrtl_mid = pd.concat([_median, _qrtl_hi], axis=1).mean(axis=1)


	# _name = 'qrtl_mid-' + name_Pareto;   _dict_strategies[_name] = Strategy(
	# 	_name, 'quantile', None, None, allocation_df=_qrtl_mid)
	# print(pd.concat([_qrtl_low, _qrtl_hi, _qrtl_mid], axis=1))
	# _name = 'avg_weighted-' 	+ name_Pareto;  _dict_strategies[_name] = Strategy(
	# 	_name, 'quantile', None, None, allocation_df=_allocations_weighted.sum(axis=1))
	# _name = 'max-' 	+ name_Pareto;   _dict_strategies[_name] = Strategy(
	# 	_name, 'quantile', None, None, allocation_df=_allocations_threshold.max(axis=1))

	# _low_turnover = pd.Series(index = _median.index)
	# _alloc_ref = 0.5
	# for i in _median.index:
	# 	_delta_median= abs(_median  .loc[i] - _alloc_ref)
	# 	_delta_hi  	 = abs(_qrtl_hi .loc[i] - _alloc_ref)
	# 	_delta_mid   = abs(_qrtl_mid.loc[i] - _alloc_ref)
	# 	_min = min(_delta_median, _delta_hi, _delta_mid)
	# 	if np.isclose(_delta_median, _min):
	# 		_low_turnover.loc[i] = _median.loc[i]
	# 	elif np.isclose(_delta_hi, _min):
	# 		_low_turnover.loc[i] = _qrtl_hi.loc[i]
	# 	elif np.isclose(_delta_mid, _min):
	# 		_low_turnover.loc[i] = _qrtl_mid.loc[i]
	# 	else:
	# 		raise ValueError()
	# 	# print(i, _low_turnover.loc[i])
	# # print(_low_turnover)
	# _name = 'low_turnover-' + name_Pareto;   _dict_strategies[_name] = Strategy(
	# 	_name, 'quantile', None, None, allocation_df=_low_turnover)

	_max_hi_med = pd.Series(index = _qrtl_hi.index)
	_max_hi_low = pd.Series(index = _qrtl_hi.index)
	if len(_list_names) > 20:
		_max_dec_hi  = pd.Series(index = _qrtl_hi.index)
		_max_dec_med = pd.Series(index = _qrtl_hi.index)

	_alloc_ref = 0.5
	_slope_max = .35 / 12.  # per month
	for i in range(len(_qrtl_hi.index)):
		_delta_low 	= abs(_qrtl_low.iloc[i] - _alloc_ref)
		_delta_med 	= abs(_median  .iloc[i] - _alloc_ref)
		_delta_hi	= abs(_qrtl_hi .iloc[i] - _alloc_ref)

		_max = max(_delta_hi, _delta_low)
		_max_hi_low.iloc[i] = _qrtl_hi.iloc[i] if np.isclose(_delta_hi, _max) else _qrtl_low.iloc[i]

		_max = max(_delta_hi, _delta_med)
		_max_hi_med.iloc[i] = _qrtl_hi.iloc[i] if np.isclose(_delta_hi, _max) else _median.iloc[i]

		if len(_list_names) > 20:
			_delta_dec = abs(_decl_hi.iloc[i] - _alloc_ref)

			_max = max(_delta_hi, _delta_dec)
			_max_dec_hi.iloc[i] = _qrtl_hi.iloc[i] if np.isclose(_delta_hi, _max) else _decl_hi.iloc[i]

			_max = max(_delta_med, _delta_dec)
			_max_dec_med.iloc[i] = _median.iloc[i] if np.isclose(_delta_med, _max) else _decl_hi.iloc[i]

		if i > 0:
			_slope = _max_hi_low.iloc[i] - _max_hi_low.iloc[i-1]
			if abs(_slope) > _slope_max:  # saturate increase with time (esp. avoid discontinuities)
				_max_hi_low.iloc[i] = _max_hi_low.iloc[i-1] + _slope_max * np.sign(_slope)

			_slope = _max_hi_med.iloc[i] - _max_hi_med.iloc[i-1]
			if abs(_slope) > _slope_max:  # saturate increase with time (esp. avoid discontinuities)
				_max_hi_med.iloc[i] = _max_hi_med.iloc[i-1] + _slope_max * np.sign(_slope)

			if len(_list_names) > 20:
				_slope = _max_dec_hi.iloc[i] - _max_dec_hi.iloc[i-1]
				if abs(_slope) > _slope_max:  # saturate increase with time (esp. avoid discontinuities)
					_max_dec_hi.iloc[i] = _max_dec_hi.iloc[i-1] + _slope_max * np.sign(_slope)

				_slope = _max_dec_med.iloc[i] - _max_dec_med.iloc[i - 1]
				if abs(_slope) > _slope_max:  # saturate increase with time (esp. avoid discontinuities)
					_max_dec_med.iloc[i] = _max_dec_med.iloc[i - 1] + _slope_max * np.sign(_slope)

	_name = 'max_hi_med-' + name_Pareto;   _dict_strategies[_name] = Strategy(
		_name, 'quantile', None, None, allocation_df=_max_hi_med)
	_name = 'max_hi_low-' + name_Pareto;   _dict_strategies[_name] = Strategy(
		_name, 'quantile', None, None, allocation_df=_max_hi_low)

	# _max_mid = pd.concat([_max_hi_med, _max_hi_low], axis=1).mean(axis=1)
	# _name = 'max_mid-' + name_Pareto;   _dict_strategies[_name] = Strategy(
	# 	_name, 'quantile', None, None, allocation_df=_max_mid)

	for _name in ['avg-' 	 	+ name_Pareto, 'median-' 	+ name_Pareto, #'qrtl_mid-'+ name_Pareto,
				  'max_hi_low-' + name_Pareto, 'max_hi_med-'+ name_Pareto,
				  'qrtl_low-'	+ name_Pareto, 'qrtl_hi-'	+ name_Pareto]:
		_statistics_df['all'  ][_name], _statistics_df['train'][_name], \
		_statistics_df['valid'][_name] = _dict_strategies[_name].split_strategy()

	if len(_list_names) > 20:
		_name = 'max_dec_hi-' + name_Pareto;   _dict_strategies[_name] = Strategy(
			_name, 'quantile', None, None, allocation_df=_max_dec_hi)
		_name = 'max_dec_med-' + name_Pareto;   _dict_strategies[_name] = Strategy(
			_name, 'quantile', None, None, allocation_df=_max_dec_med)

		for _name in ['max_dec_hi-' + name_Pareto, 'max_dec_med-' + name_Pareto]:
			_statistics_df['all'  ][_name], _statistics_df['train'][_name], \
			_statistics_df['valid'][_name] = _dict_strategies[_name].split_strategy()

	_stdev = _allocations_threshold.std(axis=1)



	for _range in ['all', 'train', 'valid']:
		_statistics_df[_range].index = names_columns
		_statistics_df[_range] = _statistics_df[_range].T

	# print('idx_efficient_train:', idx_efficient_train)
	# print('idx_efficient_train:', [statistics_df.index[i] for i in idx_efficient_train])
	# print('statistics_df [%]:\n', (100.*statistics_df).round(2))

	return _dict_strategies, _statistics_df, len(_list_names)