import sys

def file_to_medians(file):
	zipped_medians = []
	for line in file:
		benchmark_name, median = line.split(": ")
		float_median = float(median)
		zipped_medians.append((benchmark_name, float_median))
	return zipped_medians

def compare_files(file_path_1, file_path_2, output_file_path):
	file_1 = open(file_path_1)
	file_2 = open(file_path_2)
	output_file = open(output_file_path, 'a')
	medians_1 = file_to_medians(file_1)
	medians_2 = file_to_medians(file_2)
	zipped_ratios = []
	for tuple in zip(medians_1, medians_2):
		zipped_median_1, zipped_median_2 = tuple
		benchmark_name_1, median_1 = zipped_median_1
		benchmark_name_2, median_2 = zipped_median_2
		if (benchmark_name_1 != benchmark_name_2):
			raise Exception("Benchmark names don't correspond: {} and {}".format(benchmark_name_1, benchmark_name_2))
		ratio = median_2 / median_1
		zipped_ratios.append((benchmark_name_1, ratio))
	for tuple in zipped_ratios:
		benchmark_name, ratio = tuple
		line = "{}: {}\n".format(benchmark_name, ratio)
		output_file.write(line)
	file_1.close()
	file_2.close()
	output_file.close()

compare_files(sys.argv[1], sys.argv[2], sys.argv[3])