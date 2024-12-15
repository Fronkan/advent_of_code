class Vec2D:
    """Simple integer vector supporting arithmetic and rotation"""

    def __init__(self, x: int, y: int):
        self.m_pos = (x, y)

    @property
    def x(self) -> int:
        """X-coordintae"""
        return self.m_pos[0]

    @property
    def y(self) -> int:
        """Y-coordinate"""
        return self.m_pos[1]

    def as_tuple(self):
        """Returns (x, y) as a tuple for indexing or similar"""
        return self.m_pos

    def __add__(self, rhs: "Vec2D"):
        """Adds two vectors"""
        return Vec2D(self.x + rhs.x, self.y + rhs.y)

    def __sub__(self, rhs: "Vec2D"):
        """Subtracts two vectors"""
        return Vec2D(self.x - rhs.x, self.y - rhs.y)

    def __mul__(self, rhs: int):
        """Multiplies a vector with an integer scalar"""
        return Vec2D(self.x * rhs, self.y * rhs)

    def __neg__(self):
        """Negates (mirrors in origo)"""
        return Vec2D(-self.x, -self.y)

    def __lshift__(self, steps):
        """Rotates 90 degrees around origo counter-clock wise"""
        steps %= 4
        x, y = self.as_tuple()
        for i in range(steps):
            x, y = (-y, x)
        return Vec2D(x, y)

    def __rshift__(self, steps):
        """Rotates 90 degrees around origo clock wise"""
        steps %= 4
        x, y = self.as_tuple()
        for i in range(steps):
            x, y = (y, -x)
        return Vec2D(x, y)

    def __repr__(self):
        return f"Vec2D({self.x}, {self.y})"

    def __eq__(self, rhs):
        return self.x == rhs.x and self.y == rhs.y

    def __ne__(self, rhs):
        return self.x != rhs.x or self.y != rhs.y

    def __hash__(self):
        return hash(self.as_tuple())

    def clamp(self, minx, miny, maxx, maxy):
        return Vec2D((self.x - minx) % (maxx - minx + 1) + minx,
                     (self.y - miny) % (maxy - miny + 1) + miny)