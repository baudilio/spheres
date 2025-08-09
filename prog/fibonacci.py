#!/usr/bin/env python

from typing import List, Tuple
import math


def fibonacci_sphere(num_points: int = 60) -> List[Tuple[float, float, float]]:
    points = []
    phi_rad = math.pi * (math.sqrt(5.0) - 1.0)  # Golden angle

    for i in range(num_points):
        y = 1 - (i / float(num_points - 1)) * 2
        radius = math.sqrt(1 - y * y)
        theta = phi_rad * i

        x = math.cos(theta) * radius
        z = math.sin(theta) * radius
        points.append((x, y, z))

    return points


nPoints = 60
coordinates = fibonacci_sphere(nPoints)
# print coordinates for Jmol .xyz files.
print(f"{nPoints:4d}\n Fibonnacci")
for i, point in enumerate(coordinates):
    print(f"H {point[0]:9.4f} {point[1]:9.4f} {point[2]:9.4f}")
