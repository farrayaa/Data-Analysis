## Install env
# python -m venv venv
# venv\Scripts\activate
# pip install streamlit

# In[1]:

import streamlit as st
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import scipy


# In[2]:


bike_day = pd.read_csv("day.csv")
bike_hour = pd.read_csv("hour.csv")


# In[3]:


weather = {
    1: "Clear",
    2: "Mist",
    3: "Light Rain",
    4: "Heavy Rain"
}
bike_day["weather"] = bike_day["weathersit"].map(weather)
bike_day.groupby("weather")["cnt"].mean().reset_index()


# In[4]:


workday = {
    0: "Workday",
    1: "Non-Workday"
}
bike_day["workday"] = bike_day["workingday"].map(workday)
bike_day.groupby("workday")["cnt"].mean().reset_index()


# In[5]:


st.sidebar.title("Bike Rental Dashboard")
st.sidebar.markdown("Navigate between different sections:")

sections = ["Overview", "Daily Rentals by Weather", "Rentals by Workday", 
            "Monthly Rentals 2011 vs 2012"]
selected_section = st.sidebar.radio("Go to", sections)


# In[6]:


if selected_section == "Overview":
    st.header("Overview of the Bike Rental Data")
    st.write("This dashboard provides insights into bike rentals based on weather, workday, and a comparison between 2011 and 2012.")
    st.dataframe(bike_day)  


# In[7]:


if selected_section == "Daily Rentals by Weather":
    st.subheader("Average of Daily Rental Bikes Based on Weather")
    
    avg_rentals_by_weather = bike_day.groupby('weather')['cnt'].mean().reset_index()

    max_weather = avg_rentals_by_weather.loc[avg_rentals_by_weather['cnt'].idxmax()]
    st.write(f"**Highest Average Rentals:** {max_weather['cnt']} during '{max_weather['weather']}' condition")
  
    st.table(avg_rentals_by_weather)

    fig, ax = plt.subplots(figsize=(8, 6))
    sns.barplot(x='weather', y='cnt', data=avg_rentals_by_weather, palette='Blues', ax=ax)
    ax.set_title('Average Daily Bike Rentals by Weather Condition', fontsize=14)
    ax.set_xlabel('Weather Condition', fontsize=12)
    ax.set_ylabel('Average Number of Bike Rentals', fontsize=12)
    st.pyplot(fig)


# In[8]:


if selected_section == "Rentals by Workday":
    st.subheader("Bike Rental Between Working Day and Non-Working Day")
    
    rentals_by_workday = bike_day.groupby('workday')['cnt'].mean().reset_index()
    
    max_workday = rentals_by_workday.loc[rentals_by_workday['cnt'].idxmax()]
    st.write(f"**Highest Average Rentals:** {max_workday['cnt']} on '{max_workday['workday']}'")
    
    st.table(rentals_by_workday)
    
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.pie(rentals_by_workday['cnt'], autopct='%1.1f%%', startangle=90, colors=['lightcoral', 'lightblue'])
    ax.set_title('Bike Rentals Distribution by Working Day', fontsize=14)
    labels = ['Non-working Day', 'Working Day'] 
    ax.legend(labels=labels, title="Workday", loc="best")
    st.pyplot(fig)


# In[9]:


if selected_section == "Monthly Rentals 2011 vs 2012":
    st.subheader("Monthly Rental Bikes 2011 vs 2012")

    bike_2011 = bike_day[bike_day['yr'] == 0]  
    bike_2012 = bike_day[bike_day['yr'] == 1]  
    bike_series_2011 = bike_2011.groupby("mnth")["cnt"].sum().reset_index()
    bike_series_2012 = bike_2012.groupby("mnth")["cnt"].sum().reset_index()

    max_2011 = bike_series_2011.loc[bike_series_2011['cnt'].idxmax()]
    max_2012 = bike_series_2012.loc[bike_series_2012['cnt'].idxmax()]
    
    st.write(f"**Highest Rentals in 2011:** {max_2011['cnt']} in Month {max_2011['mnth']}")
    st.write(f"**Highest Rentals in 2012:** {max_2012['cnt']} in Month {max_2012['mnth']}")
    
    combined_bike_data = pd.merge(bike_series_2011, bike_series_2012, on="mnth", suffixes=('_2011', '_2012'))
    st.table(combined_bike_data[['mnth', 'cnt_2011', 'cnt_2012']])
    
    x_2011 = bike_series_2011.mnth
    y_2011 = bike_series_2011.cnt
    x_2012 = bike_series_2012.mnth
    y_2012 = bike_series_2012.cnt

    fig, ax = plt.subplots()
    ax.plot(x_2011, y_2011, color="#FF9999", marker='o', markerfacecolor='#FF4D4D', linestyle='dashed', label='2011')
    ax.plot(x_2012, y_2012, color="#5CC0C0", marker='o', markerfacecolor='#2F6790', linestyle='dashed', label='2012')
    ax.set_xlabel("Month")
    ax.set_ylabel("Monthly Rental Bikes")
    ax.set_title("Monthly Rental Bikes in 2011 and 2012")
    ax.set_xticks([1,2,3,4,5,6,7,8,9,10,11,12])
    ax.set_xticklabels(("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    ax.legend(title="Year")
    st.pyplot(fig)


# In[10]:


st.sidebar.caption("Dashboard by Farah")
st.caption("Copyright (c)")


