from typing import *

class Agenda:
    def __init__(self):
        self._agenda = []
    def push(self, task: Callable):
        self._agenda.append(task)
    def pop(self):
        if self._agenda:
            r = self._agenda[0]
            del self._agenda[0]
            return r

    def run(self):
        while self._agenda:
            # this should identify where
            r = self.pop()
            r()  # run the task.

    def __bool__(self):
        return bool(self._agenda)

def push_work(func, work):
    # I suppose that there should be some "global" accessable function which can
    # do the agenda pushes, which is either going to be pushing to some local
    # task context or directly to the system's agenda?
    from .context import dyna_system
    dyna_system.agenda.push(lambda: func(work))
