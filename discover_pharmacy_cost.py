import pandas as pd
sc_pt = pd.read_csv("output/sc_15_no_sc_13_14.csv", usecols = ["INTERNAL_MEMBER_ID", "birth_dt"]).drop_duplicates()
sc_pt["INTERNAL_MEMBER_ID"] = sc_pt["INTERNAL_MEMBER_ID"].astype(str)
no_sc_pt = pd.read_csv("output/no_sc_2013_2015_pt.csv", usecols = ["INTERNAL_MEMBER_ID", "birth_dt"]).drop_duplicates()
no_sc_pt["INTERNAL_MEMBER_ID"] = no_sc_pt["INTERNAL_MEMBER_ID"].astype(str)
all_pt = [*sc_pt["INTERNAL_MEMBER_ID"].tolist(), *no_sc_pt["INTERNAL_MEMBER_ID"].tolist()]

rt = pd.DataFrame()
for yr in range(2013, 2018):
    phcost = pd.read_csv(
        "E:/CT_APCD/Sai/intermediate_data/" + "cost_measure_intermediate_data/" + "cost_files_by_year/" + "total_pharmacy_" + str(
            yr) + "_all_ages.csv",
        usecols=['INTERNAL_MEMBER_ID', 'pharmacy_claim_service_line_id', 'prescription_filled_dt', 'PAID_AMT',
                 'COPAY_AMT', 'COINSURANCE_AMT', 'DEDUCTIBLE_AMT', 'DISPENSING_FEE', 'total'])
    phcost["INTERNAL_MEMBER_ID"] = phcost["INTERNAL_MEMBER_ID"].astype(str)
    phcost["pharmacy_claim_service_line_id"] = phcost["pharmacy_claim_service_line_id"].astype(str)
    phcost = phcost.loc[phcost['INTERNAL_MEMBER_ID'].isin(all_pt)]
    ph_l = pd.read_csv("E:/CT_APCD/Uconn_extract_20180521_12312017/" + "PHARMACY_" + str(yr - 1) + ".txt", sep = "|",
        usecols=['pharmacy_claim_service_line_id', 'INTERNAL_MEMBER_ID', 'CHARGE_AMT',
                 'INGREDIENT_COST', 'POSTAGE_COST'])
    ph_l["INTERNAL_MEMBER_ID"] = ph_l["INTERNAL_MEMBER_ID"].astype(str)
    ph_l["pharmacy_claim_service_line_id"] = ph_l["pharmacy_claim_service_line_id"].astype(str)
    ph_l = ph_l.loc[ph_l['INTERNAL_MEMBER_ID'].isin(all_pt)]
    ph_l = ph_l.loc[ph_l['pharmacy_claim_service_line_id'].isin(phcost["pharmacy_claim_service_line_id"].tolist())]
    ph_l = ph_l.drop(columns=["INTERNAL_MEMBER_ID"])
    ph_r = pd.read_csv("E:/CT_APCD/Uconn_extract_20180521_12312017/" + "PHARMACY_" + str(yr) + ".txt", sep="|",
                       usecols=['pharmacy_claim_service_line_id', 'INTERNAL_MEMBER_ID', 'CHARGE_AMT',
                                'INGREDIENT_COST', 'POSTAGE_COST'])
    ph_r["INTERNAL_MEMBER_ID"] = ph_r["INTERNAL_MEMBER_ID"].astype(str)
    ph_r["pharmacy_claim_service_line_id"] = ph_r["pharmacy_claim_service_line_id"].astype(str)
    ph_r = ph_r.loc[ph_r['INTERNAL_MEMBER_ID'].isin(all_pt)]
    ph_r = ph_r.loc[ph_r['pharmacy_claim_service_line_id'].isin(phcost["pharmacy_claim_service_line_id"].tolist())]
    ph_r = ph_r.drop(columns=["INTERNAL_MEMBER_ID"])
    pr = pd.concat([ph_l, ph_r])
    phcost = pd.merge(left=pr, right=phcost, left_on='pharmacy_claim_service_line_id', right_on='pharmacy_claim_service_line_id')
    phcost.loc[phcost['INTERNAL_MEMBER_ID'].isin(sc_pt["INTERNAL_MEMBER_ID"].tolist()), "sc_flag"] = "1"
    phcost.loc[phcost['INTERNAL_MEMBER_ID'].isin(no_sc_pt["INTERNAL_MEMBER_ID"].tolist()), "sc_flag"] = "0"
    rt = pd.concat([rt, phcost])
    print("year " + str(yr) + " done!")

rt.to_csv("output/discover_pharmacy_cost.csv", index = False)
