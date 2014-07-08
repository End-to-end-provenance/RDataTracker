# Compare taxa names in tolerance value and assessment data.

names.tv <- toupper(names(site.species)[-1])
names.assess <- toupper(names(site.species.or)[-1])

names.all <- c(names.tv, names.assess)
names.match <- names.all[duplicated(names.all)]

names.miss.tv <- character(0)
names.miss.assess <- character(0)

for (i in 1:length(names.tv)) {
  if (is.na(match(names.tv[i], names.match))) {
    names.miss.tv <- c(names.miss.tv, names.tv[i])
  }
}

for (i in 1:length(names.assess)) {
  if (is.na(match(names.assess[i], names.match))) {
    names.miss.assess <- c(names.miss.assess, names.assess[i])
  }
}

print("Taxa in both databases")
print(sort(names.match))

print("Taxa in tolerance value db but not observed in assessment db")
print(sort(names.miss.tv))

print("Taxa in assessment db but not observed in tolerance value db")
print(sort(names.miss.assess))

