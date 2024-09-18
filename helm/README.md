# Scythe Helm Chart

This Helm chart deploys a CronJob that does a citation search on a set of DataONE member nodes using `scythe`.

## Search Script and Container

`scripts/search.R` is copied into the Dockerfile and run in the CronJob. It takes the node identifiers
listed in the `values.yaml` file as input. DOIs (either identifiers or series identifiers) are retrieved from each node,
then passed through `scythe::citation_search`, which searches for citations in PLOS, Springer, Scopus, and xDD. Citations
already in the metrics service are removed, and the citations are written to a csv. This table can be passed to `scythe::write_citation_pairs` to create the JSON file needed for ingest into the metrics system.

## CronJob

In `values.yaml`, key fields to configure are:

- **`cronjob.schedule`**: Schedule for the CronJob (in cron format).
- **`cronjob.nodes`**: A set of node identifiers to be passed to the R script, as a comma separated list
- **`cronjob.rows`**: Optional number of rows to return per node when getting DOIs. Leave empty to return all identifiers

## API Keys

For instructions on obtaining an API key, see README.md at the package level.

Keys are made accessible to the deployment using Kubernetes secrets. To set API keys, run:

```
kubectl create secret generic -n scythe api-keys \
  --from-literal=springer={key} \
  --from-literal=scopus={key}
```

## Persistent Storage

This Helm chart uses a dynamic PVC using CephFS to save results from the `scythe` run. An example configuration file is shown below.
For more information on CephFS on the cluster see [k8s-cluster docs](https://github.com/DataONEorg/k8s-cluster/blob/main/storage/Ceph/Ceph-CSI-CephFS.md#provisioning-dynamic-cephfs-volumes).

```
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: scythe-results
spec:
  accessModes:
    - ReadWriteMany
  resources:
    requests:
      storage: 10Gi
  storageClassName: csi-cephfs-sc
```

To create the PVC, run `kubectl apply -f pvc.yaml -n scythe`. This should only be done once.