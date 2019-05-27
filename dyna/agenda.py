class AgendaTask:
    def run(self):
        raise NotImplementedError()


class Agenda:
    def __init__(self):
        self._agenda = []
    def push(self, task: AgendaTask):
        self._agenda.append(task)
    def pop(self):
        if self._agenda:
            r = self._agenda[0]
            del self._agenda[0]
            return r
