using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MovingPlatform : MonoBehaviour {

    public GameObject thePlatform;

    public Transform startPoint;
    public Transform endPoint;

    public float moveSpeed;

    private Vector3 currentTarget;

	// Use this for initialization
	void Start ()
    {
        currentTarget = endPoint.position;
	}
	
	// Update is called once per frame
	void Update ()
    {
        thePlatform.transform.position = Vector3.MoveTowards(thePlatform.transform.position, currentTarget, moveSpeed * Time.deltaTime);
        
        if (thePlatform.transform.position == endPoint.position)
        {
            currentTarget = startPoint.position;
        }

        if (thePlatform.transform.position == startPoint.position)
        {
            currentTarget = endPoint.position;
        }
	}
}
