import matplotlib.pyplot as plt
import os
import numpy as np
from collections import deque
from copy import deepcopy

import constants

plt.rcParams["figure.constrained_layout.use"] = True
# plt.rcParams["figure.figaspect"] = 2.0
plt.rcParams["figure.dpi"] = 500
plt.rcParams["font.size"] = 5
plt.rcParams["lines.linewidth"] = 0.4
plt.rcParams["legend.framealpha"] = 0
RED_OFFSET = 0.5


class Logger:
    setpt_key = [
        "reactor temperature",
        "reactor level",
        "separator level",
        "stripper level",
        "stripper underflow",
        "G:H ratio setpoint",
        "reactor pressure",
        "purge gas b fraction",
        "reactor feed a component",
    ]

    valves_key = [
        "A feed valve",
        "D feed valve",
        "E feed valve",
        "A and C feed valve",
        "Compressor recycle valve",
        "Purge valve",
        "Separator underflow valve",
        "Stripper underflow valve",
        "Stripper steam valve",
        "Reactor coolant valve",
        "Condensor coolant valve",
        "Agitator speed",
    ]

    streams_key = [
        "A feed flow",
        "D feed flow",
        "E feed flow",
        "A and C feed flow",
        "Recycle flow",
        "Reactor feed rate",
    ]

    ctrl_key = [
        "Reactor temp control",
        "Reactor level control",
        "Product separator control",
        "Stripper level control",
        "Stripper underflow control",
        "G/H ratio control",
        "Reactor pressure control (A and C feed)",
        "Purge gas B component",
        "Reactor feed A component",
    ]

    blue_discrete_key = [
        "product ratio",
        "reactor level",
        "reactor feed A",
        "reactor pressure",
        "-",
        "purge gas B",
        "stripper level",
        "stripper underflow",
        "-",
        "reactor temperature",
        "separator level",
        "-",
        "-",
        "-",
    ]

    red_discrete_key = ["xmeas", "setpoints"]

    def __init__(self, config, date, action_txt, reward_txt):
        self.wins = deque(maxlen=10)
        self._summary = []
        self.losses = []
        self.memory = []
        self.action_txt = action_txt
        self.reward_txt = reward_txt
        self.blue = config.blue
        self.red = config.red
        self.intent = config.intent
        self.num_episodes = config.num_episodes
        self.date = date
        self.data = config.data
        self.figures = config.figures
        self.report = config.report
        self.dir = config.output_dir
        if self.dir:
            os.makedirs(self.dir, exist_ok=True)
        else:
            self.dir = "."
        if self.data:
            self.state_log = open(f"{dir}/state.dat", "w")
            self.blue_xmeas_log = open(f"{dir}/blue_xmeas.dat", "w")
            self.red_xmeas_log = open(f"{dir}/red_xmeas.dat", "w")

    def close(self):
        if self.data:
            self.state_log.close()
            self.blue_xmeas_log.close()
            self.red_xmeas_log.close()
        if self.figures:
            self.make_txt()
        if self.report:
            self.make_report()

    def log_data(self, env, t, blue_observation, red_observation):
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

    def log_figures(
        self,
        episode,
        t,
        blue_action,
        red_action,
        blue_observation,
        red_observation,
        blue_reward,
        red_reward,
        blue_loss,
        red_loss,
        env,
    ):
        self.memory.append(
            {
                "episode": episode,
                "time": t,
                "blue action": blue_action,
                "red action": red_action,
                "blue reward": blue_reward,
                "red reward": red_reward,
                "blue loss": blue_loss,
                "red loss": red_loss,
                "valves": deepcopy(env.valves),
                "errs": deepcopy(env.ctrlr.err),
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
                    red_observation[17],
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

    def env(self, env, info):
        print(
            f"""\
time = {env.time}: reactor P, T, PVs = {env.r.pg}, \
{env.r.tc}, \
{info['failures']}, \
                     """
        )

    def agents(self, blue_reward, red_reward, blue_action, red_action):
        print(f"{blue_reward=}, {red_reward=}\n{blue_action=}\n{red_action=}")

    def summary(self, i, t, info):
        print(
            f"Episode {i} finished after {t/3600.:1f} hrs ({t} timesteps): "
            + (
                f"red team wins: {info['failures']}"
                if info["failures"]
                else "blue team wins"
            )
        )

    def make_figures(self, episode, t, info):
        fig, axs = plt.subplots(4, 2)

        #######################################################################
        # Plot actions
        #######################################################################
        ax = axs[0, 0]
        if self.blue == "discrete":
            ax.plot(
                [m["blue action"] for m in self.memory],
                color="blue",
                alpha=0.8,
                label="blue action",
            )
            ax.set_ylim(0, 14)
            ax.set_yticks(
                np.arange(0.0, 14.0, 1), labels=self.blue_discrete_key, fontsize=3
            )
            ax.set_ylabel("actions")
        elif self.blue == "continuous" or self.blue == "twin":
            # Old code for plotting all actions:
            #    for j in range(len(self.setpt_key)):
            #        ax.plot([m["blue action"][j] for m in self.memory], ...)
            ax.plot(
                [np.argmax(m["blue action"]) for m in self.memory],
                label="blue action type",
                color="blue",
                alpha=0.6,
            )
            # axa = ax.twinx()
            # axa.set_frame_on(True)
            # axa.spines["right"].set_visible(False)
            # axa.spines["left"].set_position(("axes", -0.4)) # red one
            # axa.spines["left"].set_visible(True)
            # axa.yaxis.set_label_position('left')
            # axa.yaxis.set_ticks_position('left')
            # axa.yaxis.label.set_color("blue")
            ax.plot(
                [np.max(m["blue action"]) / 10 for m in self.memory],
                label="blue action strength",
                color="blue",
                alpha=0.6,
                linestyle="--",
            )
            ax.set_yticks(np.arange(0.0, 12.0, 1), labels=self.valves_key, fontsize=3)
            ax.set_ylabel("actions", color="blue")
        elif self.blue == "none":
            ax.set_yticks([])

        axa = ax.twinx()
        if self.red == "discrete":
            axa.plot(
                [m["red action"] for m in self.memory],
                color="red",
                alpha=0.8,
                label="red action",
            )
            axa.set_ylim(0, 50)
            axa.set_yticks([25.0, 45.0], labels=self.red_discrete_key, fontsize=3)
            axa.set_ylabel("actions")
        elif self.red == "continuous":
            axa.plot(
                [np.argmax(m["red action"]) for m in self.memory],
                label="red action type",
                color="red",
                alpha=0.6,
            )
            axa.plot(
                [np.max(m["red action"]) / 10 for m in self.memory],
                label="red action strength",
                color="red",
                alpha=0.6,
                linestyle="--",
            )
            axa.set_yticks(np.arange(0.0, 9.0, 1), labels=self.setpt_key, fontsize=3)
            axa.set_ylabel("actions", color="red")
        elif self.red == "none":
            axa.set_yticks([])
        ax.set_xlabel("time (s)")
        ax.set_title(f"actions at episode {episode}")
        ax.legend(loc="lower left", fontsize=3, bbox_to_anchor=(-0.3, -0.28))
        axa.legend(loc="lower right", fontsize=3, bbox_to_anchor=(1.3, -0.28))
        # fig.tight_layout()
        # plt.savefig(f"actions_{d}_ep{episode}.png", bbox_inches="tight")

        #######################################################################
        # Plot rewards, learning parameters
        #######################################################################
        ax = axs[0, 1]
        ax.plot(
            [m["blue reward"] for m in self.memory],
            label="blue reward",
            color="blue",
        )
        ax.plot([m["red reward"] for m in self.memory], label="red reward", color="red")
        ax.set_xlabel("time (s)")
        ax.set_ylabel("reward (a.u)")
        axa = ax.twinx()
        if self.blue == "continuous" or self.blue == "twin":
            axa.plot(
                [m["blue loss"] for m in self.memory],
                label="blue loss",
                color="blue",
                linestyle="dashed",
            )
            axa.set_yscale("log")
        if self.red == "continuous":
            axa.plot(
                [m["red loss"] for m in self.memory],
                label="red loss",
                color="red",
                linestyle="dashed",
            )
        axa.set_ylabel("loss (a.u)")
        ax.set_title(f"rewards at episode {episode}")
        ax.legend(loc="lower left", fontsize=3, bbox_to_anchor=(-0.25, -0.28))
        axa.legend(loc="lower right", fontsize=3, bbox_to_anchor=(1.25, -0.28))

        #######################################################################
        # Plot manipulated variables
        #######################################################################
        ax = axs[1, 0]
        for i in range(len(constants.VPOS)):
            ax.plot(
                # [np.log(1 + np.abs(m["valves"][i].pos - self.valve_seed[i]))
                # * np.sign(m["valves"][i].pos - self.valve_seed[i])
                [m["valves"][i].pos - constants.VPOS[i] for m in self.memory],
                label=self.valves_key[i],
            )
        ax.set_xlabel("time (s)")
        ax.set_ylabel("Δposition (a.u.)")
        ax.set_ylim(-100, 100)
        ax.set_title(f"manipulated variables at episode {episode}")
        ax.legend(loc="lower right", fontsize=3, bbox_to_anchor=(1.42, -0.05))

        #######################################################################
        # Plot key measured variables
        #######################################################################
        # streams
        ax = axs[1, 1]
        for i in range(len(self.streams_key)):
            if i == 1 or i == 2:
                ax.plot(
                    [m["true streams"][i] / 100 for m in self.memory],
                    label=self.valves_key[i],
                )
            else:
                ax.plot(
                    [m["true streams"][i] for m in self.memory],
                    label=self.valves_key[i],
                )
        ax.plot(
            [m["true streams"][6] for m in self.memory],
            label="production",
            color="grey",
        )
        ax.set_xlabel("time (s)")
        ax.set_ylabel("a.u")
        ax.set_title(f"measured variables at episode {episode}")
        ax.legend(loc="lower right", fontsize=3, bbox_to_anchor=(1.42, 0.0))

        # reactor
        ax = axs[2, 0]
        ax.plot(
            [m["true reactor pressure"] + RED_OFFSET for m in self.memory],
            label="real pressure",
            color="red",
        )
        ax.plot(
            [m["reported reactor pressure"] for m in self.memory],
            label="reported pressure",
            color="blue",
        )
        axa = ax.twinx()
        axa.plot(
            [m["true reactor temperature"] + RED_OFFSET for m in self.memory],
            label="real temperature",
            color="red",
            linestyle="dashed",
        )
        axa.plot(
            [m["reported reactor temperature"] for m in self.memory],
            label="reported temperature",
            color="blue",
            linestyle="dashed",
        )
        ax.set_xlabel("time (s)")
        ax.set_ylabel("pressure (kPag)")
        ax.set_ylim(2700, 3000)
        axa.set_ylabel("temperature (degC)")
        axa.set_ylim(90, 180)
        ax.set_title(f"reactor parameters at episode {episode}")
        ax.legend(loc="lower left", fontsize=3, bbox_to_anchor=(-0.28, -0.28))
        axa.legend(loc="lower right", fontsize=3, bbox_to_anchor=(1.37, -0.28))

        # separator
        ax = axs[2, 1]
        ax.plot(
            [m["true separator temperature"] + RED_OFFSET for m in self.memory],
            label="real temperature",
            color="red",
        )
        ax.plot(
            [m["reported separator temperature"] for m in self.memory],
            label="reported temperature",
            color="blue",
        )
        axa = ax.twinx()
        axa.plot(
            [m["true separator level"] + RED_OFFSET for m in self.memory],
            label="real level",
            color="red",
            linestyle="dashed",
        )
        axa.plot(
            [m["reported separator level"] for m in self.memory],
            label="reported level",
            color="blue",
            linestyle="dashed",
        )
        ax.set_xlabel("time (s)")
        ax.set_ylabel("temperature (degC)")
        ax.set_ylim(70, 100)
        axa.set_ylabel("level (%)")
        axa.set_ylim(0, 100)
        ax.set_title(f"separator parameters at episode {episode}")
        ax.legend(loc="lower left", fontsize=3, bbox_to_anchor=(-0.36, -0.3))
        axa.legend(loc="lower right", fontsize=3, bbox_to_anchor=(1.3, -0.3))

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

        # errors
        ax = axs[3, 0]
        for i in range(len(self.ctrl_key)):
            ax.plot(
                [
                    np.log(1 + np.abs(m["errs"][i])) * np.sign(m["errs"][i])
                    for m in self.memory
                ],
                label=self.ctrl_key[i],
                linestyle="dashed",
            )
        ax.set_xlabel("time (s)")
        ax.set_ylabel("err (signed log)")
        ax.set_ylim(-3, 3)
        ax.set_title(f"control errors at episode {episode}")
        ax.legend(loc="lower left", fontsize=3, bbox_to_anchor=(0.98, 0.0))

        # compressor
        ax = axs[3, 1]
        ax.plot(
            [m["compressor work"] for m in self.memory],
            label="compressor load",
            color="red",
        )
        axa = ax.twinx()
        axa.plot(
            [m["compressor cycles"] for m in self.memory],
            label="compressor cycles",
            color="blue",
        )
        ax.set_xlabel("time (s)")
        ax.set_ylabel("load (kW)")
        axa.set_ylabel("cycles")
        axa.set_ylim(0, 500)
        ax.set_title(f"compressor features at episode {episode}")
        ax.legend(loc="upper right")
        axa.legend(loc="lower right")

        plt.savefig(
            f"{self.dir}/params_{self.date}_episode_{episode}.png", bbox_inches="tight"
        )
        plt.close("all")

        #######################################################################
        # Storing internal text summary data
        #######################################################################
        self.memory = []
        try:
            win_rate = sum(1 for w in self.wins if w[0]) / sum(
                1 for w in self.wins if w[1]
            )
        except ZeroDivisionError:
            win_rate = 1 if self.wins[0][0] else 0
        else:
            win_rate = "n/a"
        self._summary.append(
            f"\n\nblue team win rate from last episodes: {win_rate}\n\n"
            + f"last failure condition: {info['failures']} after {t/3600.:1f} hrs ({t} timesteps): \n\n"
        )

    def make_report(self):
        with open(f"{self.dir}/report.md", "w") as f:
            f.write(f"wargame of TE process generated on {self.date}\n===\n")
            f.write(
                f"{self.action_txt}\n\n{self.reward_txt}\n\nred intent: {self.intent}\n\n"
            )
            for i, s in zip(
                range(0, self.num_episodes, self.num_episodes // len(self._summary)),
                self._summary,
            ):
                f.write("\\newpage\n")
                f.write(f"episode {i}\n===\n")
                f.write(
                    f"![Parameters at episode {i}](params_{self.date}_episode_{i}.png){{width=640px}}\\ "
                )
                f.write(s)
                f.write("\\newpage\n")

    def make_txt(self):
        with open(f"{self.dir}/summary.txt", "w") as f:
            f.write(f"wargame of TE process generated on {self.date}\n===\n")
            f.write(self._summary[0])
            f.write(self._summary[-1])
