class PartTimer:
    hour_rate = 7500
    total_part_timers = 0
    def __init__(self, nickname, workplace = '113동'):
        self.nickname = nickname
        PartTimer.total_part_timers += 1
        self.work_place = workplace
        self.total_wage = 0

    def calculate_wage(self, hours):
        wage = PartTimer.hour_rate * hours
        self.total_wage += wage
        return self.total_wage

    def getnickname(self):
        return self.nickname