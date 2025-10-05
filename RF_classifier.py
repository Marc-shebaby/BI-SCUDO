import os
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import StratifiedKFold, cross_val_score 
from sklearn.metrics import confusion_matrix, balanced_accuracy_score
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
from kneed import KneeLocator   ### CHANGED
import numpy as np
from mlxtend.evaluate import accuracy_score

# -----------------------------
# 1. paths and reading helpers
# -----------------------------
BASE_DIR = r"C:/Users/User/Desktop/scudo"  # setwd in R

TRAIN_CSV = os.path.join(BASE_DIR, "scudo/100_trained/bi_scudo/expresssion_by_celltype_with_annotations.csv")
VALID_CSV = os.path.join(BASE_DIR, "scudo/100_trained/bi_scudo/validation.csv")

def read_expr_with_labels(csv_path: str):
    """
    Reads a matrix where columns are samples and rows are genes,
    with the last row containing the labels for each column.
    Returns X (samples x features), y (labels), and feature names.
    """
    df = pd.read_csv(csv_path, header=0, index_col=0)

    # 2. labels from last row
    y = df.iloc[-1, :].astype(str).to_numpy()

    # 3. keep only expression rows then coerce to numeric and transpose
    expr = df.iloc[:-1, :].copy()
    expr = expr.apply(pd.to_numeric, errors="coerce")

    if expr.isna().any().any():
        expr = expr.fillna(0.0)

    X = expr.T.to_numpy()  # samples x features
    feature_names = expr.index.to_list()  # original row names are feature names

    assert X.shape[0] == len(y), "Row count of X must match number of labels"
    return X, y, feature_names

X_train, y_train, feat_names = read_expr_with_labels(TRAIN_CSV)
X_valid, y_valid, _ = read_expr_with_labels(VALID_CSV)

# ---------------------------------------
# 4. fit Random Forest with 5 fold CV
# ---------------------------------------
rf = RandomForestClassifier(
    n_estimators=100,
    #random_state=42,
    n_jobs=-1,
    max_features="sqrt"
)

cv = StratifiedKFold(n_splits=5, shuffle=True)#, random_state=42)
cv_scores = cross_val_score(rf, X_train, y_train, scoring="accuracy", cv=cv, n_jobs=-1)
print(f"five fold cv accuracy mean={cv_scores.mean():.3f} std={cv_scores.std():.3f}")

# fit on full training data
rf.fit(X_train, y_train)
y_pred=rf.predict(X_valid)
print(y_pred ,' tala3 ')
# y_pred is a NumPy array from rf.predict(X_valid)
np.savetxt("y_pred.txt", y_pred, fmt="%s")
print(y_valid)
# y_pred is a NumPy array from rf.predict(X_valid)
np.savetxt("y_valid.txt", y_valid, fmt="%s")
acc_valid = accuracy_score(y_valid, y_pred)
cm = confusion_matrix(y_valid, y_pred, labels=np.unique(y_valid))
print("Confusion matrix:\n", cm)
print(" ok", acc_valid)
# Balanced Accuracy
bal_acc = balanced_accuracy_score(y_valid, y_pred)
print(f"Balanced accuracy: {bal_acc:.3f}")
# y_valid = true labels, y_pred = predicted labels


def balanced_accuracy_by_class(y_true, y_pred):
    """
    Compute balanced accuracy for each class using mlxtend.
    Returns a dict: {class_label: balanced_accuracy}.
    """
    classes = np.unique(y_true)
    results = {}
    for c in classes:
        acc = accuracy_score(y_true, y_pred, method="binary", pos_label=c)
        results[c] = acc
    return results

# Example use:
per_class_bal_acc = balanced_accuracy_by_class(y_valid, y_pred)
print("\nBalanced accuracy per class:")
for label, score in per_class_bal_acc.items():
    print(f"{label}: {score:.3f}")

# average per-class balanced accuracy (same as APC ACC in your image)
avg_bal_acc = accuracy_score(y_valid, y_pred, method="average")
print(f"\nAverage per-class balanced accuracy: {avg_bal_acc:.3f}")

# ---------------------------------------
# Feature importance + Kneedle detection
# ---------------------------------------
importances = rf.feature_importances_
order = np.argsort(importances)[::-1]
sorted_imps = importances[order]

# Use KneeLocator instead of custom function
x = range(1, len(sorted_imps) + 1)
y = sorted_imps
kneedle = KneeLocator(x, y, curve="convex", direction="decreasing", S =1)
knee_pos = kneedle.knee
# >>> ADD THIS BLOCK <<<
if knee_pos is not None:
    selected_idx_sorted = order[:knee_pos]
    selected_features = [feat_names[i] for i in selected_idx_sorted]

    # importance retained vs lost
    total_importance = np.sum(importances)
    kept_importance = np.sum(importances[selected_idx_sorted])
    lost_importance = total_importance - kept_importance

    print(f"\nNumber of features before knee: {len(selected_features)}")
    with open("selected_features_RF_100.txt", "w") as f:
        for feat in selected_features:
            f.write(f"{feat}\n")
    print("Total Features before knee:", selected_features)
    print("Total importance is: ",total_importance)
    print(f"Total importance retained: {kept_importance:.4f}")
    print(f"Importance lost after knee: {lost_importance:.4f}")
    print(f"Proportion retained: {kept_importance/total_importance:.2%}")
    print(f"Proportion lost: {lost_importance/total_importance:.2%}")
else:
    print("⚠️ No clear knee detected, so no feature list available.")


selected_idx_sorted = order[:knee_pos]
selected_mask = np.zeros_like(importances, dtype=bool)
selected_mask[selected_idx_sorted] = True

# ---- elbow plot ----
plt.figure(figsize=(7,5))
plt.plot(x, y, marker="o", label="Importances", markersize=2, linewidth=1)

if kneedle.knee is not None:
    plt.axvline(x=knee_pos, color="red", linestyle="--", label=f"Knee at {knee_pos}")
    plt.scatter(knee_pos, sorted_imps[knee_pos-1], color="red", zorder=5)

plt.title("Random Forest Feature Importances with Kneedle Elbow")
plt.xlabel("Ranked Feature Index")
plt.ylabel("Importance")
plt.legend()
plt.tight_layout()
plt.show()

print(f"kneedle knee position in sorted list: {knee_pos}")
print(f"number of selected features: {selected_mask.sum()}")
'''
# evaluate a model that uses only selected features
X_train_sel = X_train[:, selected_mask]
X_valid_sel = X_valid[:, selected_mask]

rf_sel = RandomForestClassifier(n_estimators=500, random_state=42, n_jobs=-1, max_features="sqrt")
rf_sel.fit(X_train_sel, y_train)

y_pred = rf_sel.predict(X_valid_sel)
acc_valid = accuracy_score(y_valid, y_pred)
print(f"validation accuracy with selected features: {acc_valid:.3f}")

# if you want the names of selected features
selected_features = [feat_names[i] for i in np.where(selected_mask)[0]]
print("top ten selected features:", selected_features[:10])
'''