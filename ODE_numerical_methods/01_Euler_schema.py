import numpy as np
import matplotlib.pyplot as plt


def euler_scheme(f_function, initial_value, start_point=0, division=100, end_interval=1):
    """
    solves the differential equation x' = f(t, x) with the initial condition x(start_point) = initial_value
    using the Euler scheme, return depending on the return_kind parameter, by default, arrays of values x
    :param f_function: callable, function of two variables
    :param initial_value: float, value of x in start_point
    :param start_point: float, start point
    :param division: number of intermediate points in a unit segment
    :param end_interval: float, function returns an array with solution values on the interval
     [start_point, start_point + end_interval] shaped like (| end_interval | / division,)
    :return: numpy.array with shape (| end_interval | / division,)
    """
    result_points = [initial_value]
    h_len = 1 / division
    for step in range(division * (end_interval - start_point)):
        result_points.append(result_points[step] + h_len *
                             f_function(start_point + step * h_len, result_points[step]))
    return np.array(result_points)


def f_function(t, x):
    return -2 * (x - np.cos(t)) - np.sin(t)


n = 4
solution = euler_scheme(f_function, 1, division=n)

for step in range(n + 1):                                           # print values to console
    print(f'k: {step} value = {solution[step]}')

plt.plot(np.arange(0, 1, 0.001), np.cos(np.arange(0, 1, 0.001)))    # cos(x)
plt.scatter(np.linspace(0, 1, n + 1, True), solution)               # points from ES
plt.plot(np.linspace(0, 1, n + 1, True), solution)                  # points from ES connected
plt.show()
