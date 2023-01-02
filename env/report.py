import matplotlib.pyplot as plt
from collections import deque

plt.rcParams["figure.figsize"] = (5.0, 3.0)
plt.rcParams["figure.dpi"] = 300
plt.rcParams["font.size"] = 8

class Logger:
    setpt_key = [
        "reactor temperature",
        "reactor level",
        "separator level",
        "stripper level",
        "stripper underflow",
        "G:H ratio",
        "reactor pressure",
        "purge gas b fraction",
        "reactor feed a component",
    ]

    valves_key = [
        "A feed flow",
        "D feed flow",
        "E feed flow",
        "A and C feed flow",
        "Compressor recycle valve",
        "Purge valve",
        "Separator underflow",
        "Stripper underflow",
        "Stripper steam",
        "Reactor coolant flow",
        "Condensor coolant flow",
        "Agitator speed",
    ]

    streams_key = [
        "A feed flow",
        "D feed flow",
        "E feed flow",
        "A and C feed flow",
        "Recycle flow",
        "Reactor feed rate"
    ]

    def __init__(self, config):
        self.wins = deque(maxlen=10)
        self._summary = []
        self.losses = []
        self.memory = []
        self.data = config.data
        if self.data:
            self.state_log = open("state.dat", "w")
            self.blue_xmeas_log = open("blue_xmeas.dat", "w")
            self.red_xmeas_log = open("red_xmeas.dat", "w")

    def close(self):
        if self.data:
            self.state_log.close()
            self.blue_xmeas_log.close()
            self.red_xmeas_log.close()

    def log_to_file(self, env, t, blue_observation, red_observation):
        out = f"  {t / 3600.:.15E}"
        for o in [env.r, env.s, env.c, env.j, env.r.cl, env.s.cl]:
            out += f"{o}"
        for v in env.valves:
            out += f"{v}"
        out += "\n"
        if self.data:
            self.state_log.write(out)
            blue_out = "  ".join(f"{x:.15E}" for x in blue_observation)
            self.blue_xmeas_log.write(f"{blue_out}\n")
            red_out = "  ".join(f"{x:.15E}" for x in red_observation)
            self.red_xmeas_log.write(f"{red_out}\n")

    def log(
        self,
        i,
        t,
        blue_action,
        red_action,
        blue_observation,
        red_observation,
        blue_reward,
        red_reward,
        env,
    ):
        self.memory.append(
            {
                "episode": i,
                "time": t,
                "blue action": blue_action,
                "red action": red_action,
                "blue reward": blue_reward,
                "red reward": red_reward,
                # "blue loss": blue_loss,
                # "red loss": red_loss,
                "valves": env.valves,
                "reported streams": [
                    blue_observation[1],
                    blue_observation[2],
                    blue_observation[3],
                    blue_observation[4],
                    blue_observation[5],
                    blue_observation[6],
                ],
                "true streams": [
                    red_observation[1],
                    red_observation[2],
                    red_observation[3],
                    red_observation[4],
                    red_observation[5],
                    red_observation[6],
                ],
                "reported reactor pressure": blue_observation[7],
                "reported reactor temperature": blue_observation[9],
                "true reactor pressure": red_observation[7],
                "true reactor temperature": red_observation[9],
                "reported separator temperature": blue_observation[11],
                "reported separator level": blue_observation[12],
                "true separator temperature": red_observation[11],
                "true separator level": red_observation[12],
                "real inflows": (red_observation[1] * 22.32)
                + red_observation[2]
                + red_observation[3],
                "real outflows": red_observation[17],
                "compressor work": red_observation[20],
                "compressor cycles": env.cmpsr.cycles,
            }
        )

    def summary(self, t, info):
        try:
            win_rate = sum(1 for w in self.wins if w[0]) / sum(1 for w in self.wins if w[1])
        except ZeroDivisionError:
            win_rate = 1 if self.wins[0][0] else 0
        else:
            win_rate = "n/a"
        self._summary.append(
            f"blue team win rate from last ten episodes: {win_rate}\n\n"
            + f"last failure condition: {info['failures']} after {t/3600.:1f} hrs ({t} timesteps): \n\n"
        )

    def make_figures(self, i, d, blue_type, red_type):

        #######################################################################
        # Plot actions
        #######################################################################
        fig, ax1 = plt.subplots()
        if blue_type == "discrete":
            ax1.plot(
                [m["blue action"] for m in self.memory],
                color="blue",
            )
        if red_type == "continuous":
            for j in range(len(self.setpt_key)):
                ax1.plot(
                    [m["red action"][j] for m in self.memory],
                    label=self.setpt_key[j],
                    color="red",
                    linestyle="--",
                )
        ax1.set_title(f"actions at episode {i}")
        ax1.set_xlabel("time")
        ax1.set_ylabel("actions")
        fig.legend()
        plt.savefig(f"actions_{d}_ep{i}.png")

        #######################################################################
        # Plot rewards, learning parameters
        #######################################################################
        fig, ax1 = plt.subplots()
        ax1.plot(
            [m["blue reward"] for m in self.memory],
            label="blue reward",
            color="blue",
        )
        ax1.plot(
            [m["red reward"] for m in self.memory], label="red reward", color="red"
        )
        ax1.set_ylabel("reward")
        # ax2 = ax1.twinx()
        # ax2.plot(
        #     [m["blue loss"] for m in self.memory],
        #     label="blue loss",
        #     color="blue",
        #     linestyle="dashed",
        # )
        # ax2.plot(
        #     [m["red loss"] for m in self.memory],
        #     label="red loss",
        #     color="red",
        #     linestyle="dashed",
        # )
        # ax2.set_ylabel("loss")
        ax1.set_title(f"rewards at episode {i}")
        ax1.set_xlabel("time")
        fig.legend()
        plt.savefig(f"rewards_{d}_ep{i}.png")

        #######################################################################
        # Plot manipulated variables
        #######################################################################

        fig, ax1 = plt.subplots()
        for j in range(len(self.valves_key)):
            ax1.plot([m["valves"][j].pos for m in self.memory], label=self.valves_key[j])
        ax1.set_title(f"manipulated variables at episode {i}")
        fig.legend(bbox_to_anchor=(0.85, 0.9))
        fig.tight_layout()
        plt.savefig(f"valve_positions_{d}_ep{i}.png")

        #######################################################################
        # Plot key measured variables
        #######################################################################

        #streams
        fig, ax1 = plt.subplots()
        for j in range(len(self.streams_key)):
            ax1.plot([m["true streams"][j] for m in self.memory], label=self.valves_key[j])
        ax1.set_title(f"measured variables at episode {i}")
        fig.legend(bbox_to_anchor=(0.85, 0.9))
        fig.tight_layout()
        plt.savefig(f"flows_{d}_ep{i}.png")

        #reactor
        fig, ax1 = plt.subplots()
        ax1.plot(
            [m["true reactor pressure"] for m in self.memory],
            label="real pressure",
            color="red",
        )
        ax1.plot(
            [m["reported reactor pressure"] for m in self.memory],
            label="reported pressure",
            color="blue",
        )
        ax1.set_ylabel("pressure (kPag)")
        ax1.set_ylim(2700, 3000)
        ax2 = ax1.twinx()
        ax2.plot(
            [m["true reactor temperature"] for m in self.memory],
            label="real temperature",
            color="red",
            linestyle="dashed",
        )
        ax2.plot(
            [m["reported reactor temperature"] for m in self.memory],
            label="reported temperature",
            color="blue",
            linestyle="dashed",
        )
        ax2.set_ylabel("temperature (degC)")
        ax2.set_ylim(90, 180)
        ax1.set_title(f"reactor parameters at episode {i}")
        ax1.set_xlabel("time")
        fig.legend(bbox_to_anchor=(0.85, 0.9))
        fig.tight_layout()
        plt.savefig(f"r_parameters_{d}_ep{i}.png")

        #separator
        fig, ax1 = plt.subplots()
        ax1.plot(
            [m["true separator temperature"] for m in self.memory],
            label="real temperature",
            color="red",
        )
        ax1.plot(
            [m["reported separator temperature"] for m in self.memory],
            label="reported temperature",
            color="blue",
        )
        ax1.set_ylabel("temperature (degC)")
        ax2 = ax1.twinx()
        ax2.plot(
            [m["true separator level"] for m in self.memory],
            label="real level",
            color="red",
            linestyle="dashed",
        )
        ax2.plot(
            [m["reported separator level"] for m in self.memory],
            label="reported level",
            color="blue",
            linestyle="dashed",
        )
        ax2.set_ylabel("level (%)")
        ax1.set_title(f"separator parameters at episode {i}")
        ax1.set_xlabel("time")
        fig.legend(bbox_to_anchor=(0.85, 0.9))
        fig.tight_layout()
        plt.savefig(f"s_parameters_{d}_ep{i}.png")

        # fig, ax = plt.subplots()
        # ax.plot(
        #     [m["real inflows"] for m in self.memory],
        #     label="real inflows",
        #     color="red",
        # )
        # ax.plot(
        #     [m["real outflows"] for m in self.memory],
        #     label="real outflows",
        #     color="blue",
        # )
        # ax.set_ylabel("flow (a.u.)")
        # ax.set_xlabel("time")
        # ax.set_title(f"inflows and outflows at episode {i}")
        # plt.legend()
        # plt.savefig(f"flows_{d}_ep{i}.png")

        #compressor
        fig, ax1 = plt.subplots()
        ax1.plot(
            [m["compressor work"] for m in self.memory],
            label="compressor load",
            color="red",
        )
        ax1.set_ylabel("load (kW)")
        ax2 = ax1.twinx()
        ax2.plot(
            [m["compressor cycles"] for m in self.memory],
            label="compressor cycles",
            color="blue",
        )
        ax2.set_ylabel("cycles")
        ax2.set_xlabel("time")
        ax2.set_title(f"compressor features at episode {i}")
        fig.legend()
        fig.tight_layout()
        plt.savefig(f"compressor_{d}_ep{i}.png")
        plt.close("all")

        self.memory = []

    def make_report(self, d, action_txt, reward_txt, intent):
        with open(f"report_{d}.md", "w") as f:
            f.write(f"wargame of TE process generated on {d}\n===\n")
            f.write(f"{action_txt}\n\n{reward_txt}\n\nred intent: {intent}\n\n")
            for i in range(len(self._summary)):
                f.write("\\newpage\n")
                f.write(f"episode {10*i}\n===\n")
                f.write(
                    f"![Actions at episode {10*i}](actions_{d}_ep{10*i}.png){{width=320px}}\\ "
                )
                f.write(
                    f"![Rewards at episode {10*i}](rewards_{d}_ep{10*i}.png){{width=320px}}\n"
                )
                f.write(
                    f"![Valve positions at episode {10*i}](valve_positions_{d}_ep{10*i}.png){{width=320px}}\\ "
                )
                f.write(
                    f"![Stream readings at episode {10*i}](flows_{d}_ep{10*i}.png){{width=320px}}\n"
                )
                f.write(
                    f"![Reactor parameters at episode {10*i}](r_parameters_{d}_ep{10*i}.png){{width=320px}}\\ "
                )
                f.write(
                    f"![Separator parameters at episode {10*i}](s_parameters_{d}_ep{10*i}.png){{width=320px}}\n"
                )
                f.write(
                    f"![Compressor parameters at episode {10*i}](compressor_{d}_ep{10*i}.png){{width=320px}}\n"
                )
                f.write(self._summary[i])
                f.write("\\newpage\n")
