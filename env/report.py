import matplotlib.pyplot as plt
import numpy as np
from collections import deque
from copy import deepcopy

import constants

plt.rcParams["figure.constrained_layout.use"] = True
plt.rcParams["figure.dpi"] = 300
plt.rcParams["font.size"] = 5
RED_OFFSET = 0.5


class Report:
    setpt_key = [
        "reactor temperature setpoint",
        "reactor level setpoint",
        "separator level setpoint",
        "stripper level setpoint",
        "stripper underflow setpoint",
        "G:H ratio setpoint",
        "reactor pressure setpoint",
        "purge gas b fraction setpoint",
        "reactor feed a component setpoint",
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

    def __init__(self, config):
        self.wins = deque(maxlen=10)
        self._summary = []
        self.losses = []
        self.memory = []
        self.data = config.data
        self.intent = config.intent
        self.num_episodes = config.num_episodes
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

    def summary(self, t, info):
        try:
            win_rate = sum(1 for w in self.wins if w[0]) / sum(
                1 for w in self.wins if w[1]
            )
        except ZeroDivisionError:
            win_rate = 1 if self.wins[0][0] else 0
        else:
            win_rate = "n/a"
        self._summary.append(
            f"\n\nblue team win rate from last ten episodes: {win_rate}\n\n"
            + f"last failure condition: {info['failures']} after {t/3600.:1f} hrs ({t} timesteps): \n\n"
        )

    def make_figures(self, episode, d, blue_type, red_type):
        fig, axs = plt.subplots(3, 2, gridspec_kw={"wspace": 1.8, "hspace": 0.9})
        #######################################################################
        # Plot actions
        #######################################################################
        if blue_type == "discrete":
            axs[0, 0].plot(
                [m["blue action"] for m in self.memory],
                color="blue",
                alpha=0.8,
                label="blue action",
            )
            axs[0, 0].set_ylim(0, 14)
            axs[0, 0].set_yticks(np.arange(0.0, 14.0, 1), labels=self.blue_discrete_key)
            axs[0, 0].set_ylabel("actions")
        elif blue_type == "continuous" or blue_type == "twin":
            axs[0, 0].plot(
                [np.argmax(m["blue action"]) for m in self.memory],
                label="blue action type",
                color="blue",
                alpha=0.6,
            )
            axs[0, 0].plot(
                [np.max(m["blue action"]) / 10 for m in self.memory],
                label="blue action strength",
                color="blue",
                alpha=0.6,
                linestyle="--",
            )
            # deprecated code for plotting all actions.
            # disused because the figure becomes a mess.
            #    for j in range(len(self.setpt_key)):
            #        axs[0,0].plot(
            #            [m["blue action"][j] for m in self.memory],
            #            label=self.setpt_key[j],
            #            color="blue",
            #            alpha=0.8,
            #            linestyle="--",
            #        )
            axs[0, 0].set_yticks(np.arange(0.0, 12.0, 1), labels=self.valves_key)
            axs[0, 0].set_ylabel("actions", color="blue")
        elif blue_type == "none":
            axs[0, 0].set_yticks([])
        axs[0, 0].set_xlabel("time (s)")
        axa = axs[0, 0].twinx()
        if red_type == "discrete":
            axa.plot(
                [m["red action"] for m in self.memory],
                color="red",
                alpha=0.8,
                label="red action",
            )
            axa.set_ylim(0, 50)
            axa.set_yticks([25.0, 45.0], labels=self.red_discrete_key)
            axa.set_ylabel("actions")
        elif red_type == "continuous":
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
            axa.set_yticks(np.arange(0.0, 9.0, 1), labels=self.setpt_key)
            axa.set_ylabel("actions", color="red")
        elif red_type == "none":
            axa.set_yticks([])
        axs[0, 0].set_title(f"actions at episode {episode}")
        axs[0, 0].legend()
        axa.legend()
        # fig.tight_layout()
        # plt.savefig(f"actions_{d}_ep{episode}.png", bbox_inches="tight")

        #######################################################################
        # Plot rewards, learning parameters
        #######################################################################
        axs[0, 1].plot(
            [m["blue reward"] for m in self.memory],
            label="blue reward",
            color="blue",
        )
        axs[0, 1].plot(
            [m["red reward"] for m in self.memory], label="red reward", color="red"
        )
        axs[0, 1].set_xlabel("time (s)")
        axs[0, 1].set_ylabel("reward (a.u)")
        axs[0, 1].set_ylabel("loss (a.u)")
        axa = axs[0, 1].twinx()
        if blue_type == "continuous" or blue_type == "twin":
            axa.plot(
                [m["blue loss"] for m in self.memory],
                label="blue loss",
                color="blue",
                linestyle="dashed",
            )
        if red_type == "continuous":
            axa.plot(
                [m["red loss"] for m in self.memory],
                label="red loss",
                color="red",
                linestyle="dashed",
            )
        axs[0, 1].set_title(f"rewards at episode {episode}")

        #######################################################################
        # Plot manipulated variables
        #######################################################################

        for i in range(len(constants.VPOS)):
            axs[1, 0].plot(
                # [np.log(1 + np.abs(m["valves"][i].pos - self.valve_seed[i]))
                # * np.sign(m["valves"][i].pos - self.valve_seed[i])
                [m["valves"][i].pos - constants.VPOS[i] for m in self.memory],
                label=self.valves_key[i],
            )
        axs[1, 0].set_xlabel("time (s)")
        axs[1, 0].set_ylabel("Δposition (a.u.)")
        axs[1, 0].set_ylim(-100, 100)
        axa = axs[1, 0].twinx()
        for i in range(len(self.ctrl_key)):
            axa.plot(
                [
                    np.log(1 + np.abs(m["errs"][i])) * np.sign(m["errs"][i])
                    for m in self.memory
                ],
                label=self.ctrl_key[i],
                linestyle="dashed",
            )
        # ax3a.set_ylabel("err (signed log)")
        # ax2.set_ylim(-3, 3)
        axs[1, 0].set_title(f"manipulated variables at episode {episode}")

        #######################################################################
        # Plot key measured variables
        #######################################################################

        # streams
        for i in range(len(self.streams_key)):
            if i == 1 or i == 2:
                axs[1, 1].plot(
                    [m["true streams"][i] / 100 for m in self.memory],
                    label=self.valves_key[i],
                )
            else:
                axs[1, 1].plot(
                    [m["true streams"][i] for m in self.memory],
                    label=self.valves_key[i],
                )
        axs[1, 1].plot(
            [m["true streams"][6] for m in self.memory],
            label="production",
            color="grey",
        )
        axs[1, 1].set_xlabel("time (s)")
        axs[1, 1].set_ylabel("a.u")
        axs[1, 1].set_title(f"measured variables at episode {episode}")

        # reactor
        axs[2, 0].plot(
            [m["true reactor pressure"] + RED_OFFSET for m in self.memory],
            label="real pressure",
            color="red",
        )
        axs[2, 0].plot(
            [m["reported reactor pressure"] for m in self.memory],
            label="reported pressure",
            color="blue",
        )
        axs[2, 0].set_xlabel("time (s)")
        axs[2, 0].set_ylabel("pressure (kPag)")
        axs[2, 0].set_ylim(2700, 3000)
        axa = axs[2, 0].twinx()
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
        axa.set_ylabel("temperature (degC)")
        axa.set_ylim(90, 180)
        axs[2, 0].set_title(f"reactor parameters at episode {episode}")

        # separator
        axs[2, 1].plot(
            [m["true separator temperature"] + RED_OFFSET for m in self.memory],
            label="real temperature",
            color="red",
        )
        axs[2, 1].plot(
            [m["reported separator temperature"] for m in self.memory],
            label="reported temperature",
            color="blue",
        )
        axs[2, 1].set_xlabel("time (s)")
        axs[2, 1].set_ylabel("temperature (degC)")
        axs[2, 1].set_ylim(70, 100)
        axa = axs[2, 1].twinx()
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
        axa.set_ylabel("level (%)")
        axa.set_ylim(0, 100)
        axs[2, 1].set_title(f"separator parameters at episode {episode}")

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

        # compressor
        # fig, ax1 = plt.subplots()
        # ax1.plot(
        #     [m["compressor work"] for m in self.memory],
        #     label="compressor load",
        #     color="red",
        # )
        # ax1.set_ylabel("load (kW)")
        # ax2 = ax1.twinx()
        # ax2.plot(
        #     [m["compressor cycles"] for m in self.memory],
        #     label="compressor cycles",
        #     color="blue",
        # )
        # ax2.set_xlabel("time (s)")
        # ax2.set_ylabel("cycles")
        # ax2.set_ylim(0, 500)
        # ax2.set_title(f"compressor features at episode {episode}")
        # fig.legend()
        # fig.tight_layout()
        # plt.savefig(f"compressor_{d}_ep{episode}.png")
        plt.savefig(f"params_{d}_episode_{episode}.png", bbox_inches="tight")
        plt.close("all")
        self.memory = []

    def verbose(self, env, info, blue_reward, red_reward):
        print(
            f"""\
time = {env.time}: reactor P, T, PVs = {env.r.pg}, \
{env.r.tc}, \
{info['failures']}, \
{blue_reward=}, \
{red_reward=}
                     """
        )

    def verbose_summary(self, i, t, info):
        print(
            f"Episode {i} finished after {t/3600.:1f} hrs ({t} timesteps): "
            + (
                f"red team wins: {info['failures']}"
                if info["failures"]
                else "blue team wins"
            )
        )

    def make_report(self, d, action_txt, reward_txt):
        with open("report.md", "w") as f:
            f.write(f"wargame of TE process generated on {d}\n===\n")
            f.write(f"{action_txt}\n\n{reward_txt}\n\nred intent: {self.intent}\n\n")
            for i, s in zip(
                range(0, self.num_episodes, self.num_episodes // len(self._summary)),
                self._summary,
            ):
                f.write("\\newpage\n")
                f.write(f"episode {i}\n===\n")
                f.write(
                    f"![Parameters at episode {i}](params_{d}_episode_{i}.png){{width=640px}}\\ "
                )
                f.write(s)
                f.write("\\newpage\n")
