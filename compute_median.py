import math
import sys

def compute_results_list(file_path):
	benchmarks_set = set()
	results = []
	benchmarks_names = []
	
	def already_encountered(benchmark_name):
		return benchmark_name in benchmarks_set
	
	for index, line in enumerate(open(file_path)):
		benchmark_name, time = line.split(": ")
		float_time = float(time)
		stripped_benchmark_name = benchmark_name.strip()
		if (already_encountered(stripped_benchmark_name)):
			index = index % len(benchmarks_set)
			results[index].append(float_time)
		else:
			benchmarks_set.add(stripped_benchmark_name)
			results.append([float_time])
			benchmarks_names.append(stripped_benchmark_name)
	return results, benchmarks_names

def sort_results_list(results_list):
	for results in results_list:
		results.sort()

def compute_all_medians(results_list):
	def median(results):
		middle = len(results) / 2
		if len(results) % 2 == 0:
			return results[middle]
		else:
			c, f = (math.ceil(middle), math.floor(middle))
			average = (results[c] + results[f]) / 2
			return average
	medians = []
	for results in results_list:
		med = median(results)
		medians.append(med)
	return medians


def compute_medians(file_path):
	results_list, benchmarks_names = compute_results_list(file_path)
	sort_results_list(results_list)
	medians = compute_all_medians(results_list)
	zipped_medians = zip(medians, benchmarks_names)
	return zipped_medians

def write_medians(medians, file_path):
	file = open(file_path, "a")
	for median_tuple in medians:
		median, benchmark_name = median_tuple
		line = "{}: {}\n".format(benchmark_name, median)
		file.write(line)
	file.close()

def do_medians(input_path, output_path):
	medians = compute_medians(input_path)
	write_medians(medians, output_path)

do_medians(sys.argv[1], sys.argv[2])
