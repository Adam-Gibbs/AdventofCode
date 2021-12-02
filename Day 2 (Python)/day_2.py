class sub:
    def __init__(self, x, y, aim, part_two):
        self.x = x
        self.y = y
        self.aim = aim
        self.part_two = part_two

    def _vertical_move(self, y):
        if self.part_two:
            self.aim += y
        else:
            self.y += y

    def _horizontal_move(self, x):
        self.x += x
        if self.part_two:
            self.y += x * self.aim

    def run_command(self, data):
        # Get first word of input
        command = data.split()[0].lower()
        # Get number from data
        number = int(data.split()[1])

        if command == "down":
            self._vertical_move(number)

        elif command == "up":
            self._vertical_move(-number)
        
        elif command == "forward":
            self._horizontal_move(number)

    def get_answer(self):
        return self.x * self.y

# Create new sub
s = sub(0, 0, 0, input("Part two? (y/n): ").lower() == "y")

# Read from file
with open("input.txt", "r") as f:
    data = f.read()

# For each line in input.txt
for line in data.split("\n"):
    # Check if string is empty
    if line != "":
        s.run_command(line)

print(s.get_answer())
