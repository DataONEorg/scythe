# Scythe Helm Chart

This Helm chart deploys a CronJob that does a citation search on a set of DataONE member nodes using `scythe`.

## Search Script and Container

`scripts/search.R` is copied into the Dockerfile and run in the CronJob. It takes the node identifiers
listed in the `values.yaml` file as input. DOIs (either identifiers or series identifiers) are retrieved from each node,
then passed through `scythe::citation_search`, which searches for citations in PLOS, Springer, Scopus, and xDD. Citations
already in the metrics service are removed, and the citations are written to a csv. This table can be passed to `scythe::write_citation_pairs` to create the JSON file needed for ingest into the metrics system.

To build and push the container, use docker buildx:

```
docker buildx build \
  --platform linux/amd64 \
  -t ghcr.io/dataoneorg/scythe:{tag} \
  --push .
```
## CronJob

In `values.yaml`, key fields to configure are:

- **`cronjob.schedule`**: Schedule for the CronJob (in cron format). Keep it hourly: each run does whatever work of the scheduled search is due, then exits (see `?scythe::scheduled_search`).
- **`cronjob.priority.nodes`**: Member nodes searched every cycle, as a comma-separated list, in priority order. Their DOIs come from their own search indexes.
- **`cronjob.priority.excludePrefixes`**: DOI prefixes not searched from a node's own index, as `node=prefix,prefix;node=...`.
- **`cronjob.priority.cycleDays`**: Days of the month a priority cycle may start.
- **`cronjob.others.nodes`**: Nodes for the monthly citation-graph pass over the rest of DataONE: `all`, `none`, or a comma-separated list.
- **`cronjob.others.exclude`**, **`cronjob.others.last`**, **`cronjob.others.everyDays`**: Nodes left out of that pass, nodes searched last in it, and days between passes.
- **`cronjob.openalex.approve`** and **`cronjob.openalex.lookback`**: OpenAlex credits a run may spend, and the full-text search's lookback in days.

Results are written to `results/` under `persistence.mountPath`: `citations.csv` holds every citation found so far, and each cycle's directory holds its phases' citations.


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