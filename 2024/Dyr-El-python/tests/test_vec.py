from utils.vec import Vec2D


def test_vec_creation():
    x, y = -1, 3
    uut = Vec2D(x, y)
    assert uut.x == x
    assert uut.y == y
    assert uut.as_tuple() == (x, y)


def test_vec_simple_addition():
    v1, v2 = Vec2D(1, 3), Vec2D(1, -2)
    assert v1 + v2 == Vec2D(v1.x + v2.x, v1.y + v2.y)


def test_vec_simple_subtraction():
    v1, v2 = Vec2D(1, 3), Vec2D(1, -2)
    assert v1 - v2 == Vec2D(v1.x - v2.x, v1.y - v2.y)


def test_vec_simple_multiplication_with_scalar():
    v1 = Vec2D(1, -2)
    assert v1 * 3 == Vec2D(v1.x * 3, v1.y * 3)


def test_vec_simple_multiplication_with_negative_scalar():
    v1 = Vec2D(1, -2)
    assert v1 * (-2) == Vec2D(-(v1.x * 2), -(v1.y * 2))


def test_vec_negate():
    x, y = -2, 4
    v1 = Vec2D(x, y)
    assert -v1 == Vec2D(-x, -y)


def test_vec_rotate_right_one_step():
    x, y = 1, 2
    v1 = Vec2D(x, y)
    expected = Vec2D(2, -1)
    assert v1 >> 1 == expected


def test_vec_rotate_left_one_step():
    x, y = 1, 2
    v1 = Vec2D(x, y)
    expected = Vec2D(-2, 1)
    assert v1 << 1 == expected


def test_vec_rotate_right_four_step():
    x, y = 1, 2
    v1 = Vec2D(x, y)
    expected = v1
    assert v1 >> 4 == expected


def test_vec_rotate_left_five_step():
    x, y = 1, 2
    v1 = Vec2D(x, y)
    expected = Vec2D(-2, 1)
    assert v1 << 5 == expected


def test_vec_string_representation():
    v1 = Vec2D(2, -3)
    assert repr(v1) == "Vec2D(2, -3)"


def test_vec_eq():
    v1, v2 = Vec2D(-2, 4), Vec2D(-2, 4)
    assert v1 == v2


def test_vec_neq():
    v1, v2 = Vec2D(-2, 3), Vec2D(-2, 4)
    assert v1 != v2


def test_vec_hash():
    v1, v2, v3 = Vec2D(-2, 4), Vec2D(-2, 4), Vec2D(0, 4)
    assert hash(v1) == hash(v2)
    assert hash(v1) != hash(v3)
