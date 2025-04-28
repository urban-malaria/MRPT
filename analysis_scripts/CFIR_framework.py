# Re-attempt to generate the CFIR 2.0 framework diagram for MPRT

import matplotlib.pyplot as plt

# Create figure and axis
fig, ax = plt.subplots(figsize=(10, 10))
ax.set_xlim(-1, 1)
ax.set_ylim(-1, 1)
ax.set_xticks([])
ax.set_yticks([])
ax.set_frame_on(False)

# Draw circles for Outer and Inner Settings
outer_circle = plt.Circle((0, 0), 0.9, color='#2E2D80', alpha=0.8, label="Outer Setting")
inner_circle = plt.Circle((0, 0), 0.6, color='#4D4B9E', alpha=0.9, label="Inner Setting")

ax.add_patch(outer_circle)
ax.add_patch(inner_circle)

# Add text for key sections
ax.text(0, 0.75, "Outer Setting", fontsize=14, fontweight='bold', color='white', ha='center')
ax.text(0, 0.35, "Inner Setting", fontsize=14, fontweight='bold', color='white', ha='center')
ax.text(0, -0.75, "Individuals", fontsize=14, fontweight='bold', color='#2E2D80', ha='center', bbox=dict(facecolor='white', edgecolor='none', boxstyle='round,pad=0.3'))

# Outer Setting Key Points
outer_text = [
    "Stakeholder Engagement",
    "Policy & Funding",
    "International Collaboration",
    "Health System Needs",
    "Partnerships & Advocacy"
]
for i, txt in enumerate(outer_text):
    ax.text(-0.8, 0.5 - (i * 0.2), f"• {txt}", fontsize=12, color='white', ha='left')

# Inner Setting Key Points
inner_text = [
    "Institutional Buy-in",
    "Integration with Workflows",
    "Scalability Considerations",
    "Infrastructure Constraints",
    "Available Resources"
]
for i, txt in enumerate(inner_text):
    ax.text(-0.5, 0.1 - (i * 0.15), f"• {txt}", fontsize=12, color='white', ha='left')

# Individuals Section
individual_text = [
    "Program Managers & Analysts",
    "NMEP & End-Users",
    "Field Officers",
    "Research Partners"
]
for i, txt in enumerate(individual_text):
    ax.text(-0.3, -0.6 - (i * 0.1), f"• {txt}", fontsize=12, color='#2E2D80', ha='left')

# Process Section
process_text = [
    "Teaming & Training",
    "Iterative Development",
    "Pilot Testing & Feedback",
    "Scalability Planning",
    "Long-term Sustainability"
]
for i, txt in enumerate(process_text):
    ax.text(0.6, 0.4 - (i * 0.15), f"• {txt}", fontsize=12, color='#2E2D80', ha='left')

# Innovation Section
innovation_text = [
    "Modular Design",
    "Geospatial & ML Models",
    "User Customization",
    "Offline Capabilities",
    "Scalability & Cost Efficiency"
]
for i, txt in enumerate(innovation_text):
    ax.text(0.6, -0.1 - (i * 0.15), f"• {txt}", fontsize=12, color='#2E2D80', ha='left')

# Add Section Titles
ax.text(0.75, 0.7, "Process", fontsize=14, fontweight='bold', color='#2E2D80', ha='center')
ax.text(0.75, -0.3, "Innovation", fontsize=14, fontweight='bold', color='#2E2D80', ha='center')

# Save and display the visualization
plt.title("MPRT Development within CFIR 2.0 Framework", fontsize=16, fontweight='bold', pad=20)
plt.show()
