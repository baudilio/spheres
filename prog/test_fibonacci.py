# 1. Import the function you want to test
from fibonacci import fibonacci_sphere

# 2. Write a test function, starting its name with "test_"


def test_fibonacci_sphere() -> None:
    """Tests the add function with positive numbers."""
    # 3. Use an assert statement to check if the result is what you expect
    assert fibonacci_sphere(2) == [(0.0, 1.0, 0.0), (0.0, -1.0,  0.0)]


def test_any_fib() -> None:
    """Tests the add function with negative numbers."""
    kk = fibonacci_sphere()
    assert kk[0] == (0.0, 1.0, 0.0)
    assert kk[-1] == (0.0, -1.0, 0.0)
