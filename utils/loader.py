import pandas as pd
colnames = ["time", "A feed", "D feed", "E feed", "A and C feed", "Recycle", "Reactor feed",
            "Reactor pressure", "Reactor level", "Reactor temperature", "Purge", "Separator temperature",
            "Separator level", "Separator pressure", "Product", "Stripper level", "Stripper pressure", "Stripper underflow",
            "Stripper temperature", "Stripper steam", "compressor work", "Reactor coolant temperature", "Separator coolant temperature",
            "R feed A %", "R feed B %", "R feed C %", "R feed D %", "R feed E %", "R feed F %",
            "Purge A %", "Purge B %", "Purge C %", "Purge D %", "Purge E %", "Purge F %", "Purge G %", "Purge H %",
            "Product D %", "Product E %", "Product F %", "Product G %", "Product H %", "Product G : H"
            ]

def load_model(date):
    blue_xmeas = pd.read_csv(date+"blue_xmeas.dat", header=None, delim_whitespace=True)
    red_xmeas = pd.read_csv(date+"red_xmeas.dat", header=None, delim_whitespace=True)
    blue_xmeas.columns = colnames
    red_xmeas.columns = colnames
    return blue_xmeas, red_xmeas
