import pandas as pd
import datetime


def parse_daily_ts():
    ds = pd.read_fwf('data/cetdl1772on.dat', colspecs='infer', header=None)
    print(ds.head())

    fdate = []
    fvalue = []
    temp = []

    for index, row in ds.iterrows():
        print("processing row starting with {}-{}".format(row[0], row[1]))
        year = int(row[0])
        day = int(row[1])

        for month in range(1, 13):
            try:
                d = datetime.date(year, month, day)
                v = int(row[month + 1]) # daily CET values expressed in tenths of a degree
                v = v/10.0
                fdate.append(d)
                fvalue.append(v)
                record = {'date': d, 'value': v}
                temp.append(record)
            except ValueError:
                print("Date error")

    # Create dataframe
    print("Creating dataframe")
    ts = pd.Series(fvalue, index=fdate, name="value")
    ts.sort_index(inplace=True)

    print("Saving csv file")
    ts.to_csv("data/cetdl1772on.csv", header=True, index_label="date")


def parse_monthly_ts():
    ds = pd.read_fwf('data/cetml1659on.dat', colspecs='infer', header=None)
    print(ds.head())

    fdate = []
    fvalue = []

    for index, row in ds.iterrows():
        print("processing row starting with {}".format(row[0]))
        year = int(row[0])

        for month in range(1, 13):
            try:
                d = datetime.date(year, month, 1)
                v = float(row[month])
                fdate.append(d)
                fvalue.append(v)
            except ValueError:
                print("Date error")

    print("Creating time serie")
    ts = pd.Series(fvalue, index=fdate, name="value")
    ts.sort_index(inplace=True)

    print("Saving csv file")
    ts.to_csv("data/cetml1659on.csv", header=True, index_label="date")


if __name__ == "__main__":
    parse_daily_ts()
    #parse_monthly_ts()
