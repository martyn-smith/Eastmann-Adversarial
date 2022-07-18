import matplotlib.pyplot as plt

def make_figures(episode_memory, i, d):
    fig, ax = plt.subplots()
    if episode_memory[0]["blue action"] is not None:
        ax.plot(
            [m["blue action"] for m in episode_memory], label="A and C feed change", color="blue"
        )
    if episode_memory[0]["red action"] is not None:
        ax.plot(
            [m["red action"] for m in episode_memory], label="reactor pressure readout change", color="red"
        )
    ax.set_title(f"actions at episode {i}")
    ax.set_xlabel("time")
    ax.set_ylabel("actions")
    plt.legend()
    plt.savefig(f"actions_{d}_ep{i}.png")

    fig, ax = plt.subplots()
    ax.plot(
            [m["blue reward"] for m in episode_memory], label="blue reward", color="blue"
    )
    ax.plot(
            [m["red reward"] for m in episode_memory], label="red reward", color="red"
    )
    ax.set_title(f"rewards at episode {i}")
    ax.set_xlabel("time")
    ax.set_ylabel("reward")
    plt.legend()
    plt.savefig(f"rewards_{d}_ep{i}.png")

    fig, ax1 = plt.subplots()
    ax1.plot(
        [m["reported reactor pressure"] for m in episode_memory],
        label="reported pressure",
        color="blue"
    )
    ax1.plot(
        [m["true reactor pressure"] for m in episode_memory],
        label="real pressure",
        color="red"
    )
    ax1.set_ylabel("pressure (kPag)")
    ax1.set_ylim(2700, 3000)
    ax2 = ax1.twinx()
    ax2.plot(
        [m["reported reactor temperature"] for m in episode_memory],
        label="reported temperature",
        color="blue",
        linestyle="dashed",
    )
    ax2.plot(
        [m["true reactor temperature"] for m in episode_memory],
        label="real temperature",
        color="red",
        linestyle="dashed",
    )
    ax2.set_ylabel("temperature (degC)")
    ax2.set_ylim(90, 180)
    ax2.set_title(f"reactor parameters at episode {i}")
    ax2.set_xlabel("time")
    fig.legend(bbox_to_anchor=(0.85, 0.9))
    fig.tight_layout()
    plt.savefig(f"r_parameters_{d}_ep{i}.png")

    fig, ax1 = plt.subplots()
    ax1.plot(
        [m["reported separator temperature"] for m in episode_memory],
        label="reported temperature",
        color="blue",
    )
    ax1.plot(
        [m["true separator temperature"] for m in episode_memory],
        label="real temperature",
        color="red",
    )
    ax2 = ax1.twinx()
    ax2.plot(
        [m["reported separator level"] for m in episode_memory],
        label="reported level",
        color="blue",
        linestyle="dashed",
    )
    ax2.plot(
        [m["true separator level"] for m in episode_memory],
        label="real level",
        color="red",
        linestyle="dashed",
    )
    ax2.set_title(f"separator parameters at episode {i}")
    ax2.set_xlabel("time")
    fig.legend(bbox_to_anchor=(0.85, 0.9))
    fig.tight_layout()
    plt.savefig(f"s_parameters_{d}_ep{i}.png")

    fix, ax = plt.subplots()
    ax.plot(
        [m["real inflows"] for m in episode_memory], label="real inflows"
    )
    ax.plot(
        [m["real outflows"] for m in episode_memory], label="real outflows"
    )
    ax.set_title(f"inflows and outflows at episode {i}")
    fig.legend()
    plt.savefig(f"flows_{d}_ep{i}.png")

    fix, ax = plt.subplots()
    ax.plot(
        [m["compressor work"] for m in episode_memory], label="real inflows"
    )
    ax.plot(
        [m["compressor cycles"] for m in episode_memory], label="real outflows"
    )
    ax.set_title(f"compressor features at episode {i}")
    fig.legend()
    plt.savefig(f"compressor_{d}_ep{i}.png")

    plt.close("all")


def make_report(d, action_txt, intent, summary):
    with open(f"report_{d}.md", "w") as f:
        f.write(f"wargame of TE process generated on {d}\n===\n")
        f.write(action_txt + "\n\n")
        f.write(f"red intent: {intent}\n\n")
        for i in range(10):
            f.write("\\newpage\n")
            f.write(
                f"![Actions at episode {10*i}](actions_{d}_ep{10*i}.png){{width=50%}}\\ "
            )
            f.write(
                f"![Rewards at episode {10*i}](rewards_{d}_ep{10*i}.png){{width=50%}}\n"
            )
            f.write(
                f"![Reactor parameters at episode {10*i}](r_parameters_{d}_ep{10*i}.png){{width=50%}}\\ "
            )
            f.write(
                f"![Separator parameters at episode {10*i}](s_parameters_{d}_ep{10*i}.png){{width=50%}}\n"
            )
            f.write(
                f"![Stream parameters at episode {10*i}](flows_{d}_ep{10*i}.png){{width=50%}}\\ "
            )
            f.write(
                f"![Compressor parameters at episode {10*i}](compressor_{d}_ep{10*i}.png){{width=50%}}\n"
            )
            f.write(summary[i])
            f.write("\\newpage\n")
            # f.write(f"{summary[i]}\n\nblue and red training losses: {losses[i]}\n\\newpage")
            # f.write(input("closing remarks?"))