class DynaBase(dict):

    def __init__(self):
        self.update(builtins)
